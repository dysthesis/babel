# From: https://github.com/emacs-twist/twist.nix/blob/b023e015963226157b6c8c09286e6067e5ca386d/pkgs/emacs/build/default.nix
lib: {
  pkgs,
  emacs ? pkgs.emacs,
  ename,
  src,
  version ? null,
  files,
  lispFiles,
  meta,
  nativeCompileAhead,
  wantExtraOutputs,
  elispInputs,
  dontByteCompile ? false,
  ...
} @ attrs:
with builtins; let
  inherit
    (pkgs)
    texinfo
    gnumake
    writeText
    ;

  compNative =
    writeText "comp-native.el"
    /*
    elisp
    */
    ''
      ;;; --- -*- lexical-binding: t -*-

      ;; Compile all Emacs Lisp files in the default directory.
      ;;
      ;; Based on code from https://www.emacswiki.org/emacs/GccEmacs#h5o-14

      (require 'comp-run)

      (if (fboundp 'comp--async-runnings)
          (defalias 'twist--comp-async-runnings #'comp--async-runnings)
        (defalias 'twist--comp-async-runnings #'comp-async-runnings))

      (defun run-native-compile-sync ()
        (native-compile-async (or (pop command-line-args-left)
                                  (error "Specify a source directory as the argument"))
                              nil nil
                              (lambda (name)
                                (and (string-match-p "^[^.]" (file-name-nondirectory name))
                                     (not (string-suffix-p "-pkg.el" name)))))
        (while (or comp-files-queue
                   (> (twist--comp-async-runnings) 0))
          ;; Calibration may be needed
          (sleep-for 0.3)))
    '';
  nativeComp = emacs.withNativeCompilation or emacs.nativeComp or false;

  regex = ".*/([^/]+)";

  stringBaseName = file:
    if match regex file != null
    then head (match regex file)
    else file;

  hasFile = pred: (lib.findFirst pred null (attrNames files) != null);

  canProduceInfo = hasFile (f: match ".+\\.(info|texi(nfo)?)" f != null);

  copySourceCommand = concatStringsSep "\n" (lib.mapAttrsToList
    (
      origin: dest: ''
        mkdir -p $(dirname "build/${dest}")
        cp -r "$src/${origin}" "build/${dest}"
      ''
    )
    files);
in
  stdenv.mkDerivation {
    inherit src ename meta;
    # Add a fake version if the version is null
    version =
      if version == null
      then "0.0.0"
      else version;

    pname = concatStringsSep "-" [
      (replaceStrings ["."] ["-"] emacs.name)
      (lib.toPName ename)
    ];

    outputs =
      ["out"]
      ++ lib.optional (wantExtraOutputs && canProduceInfo) "info";

    buildInputs = [emacs gnumake] ++ lib.optional wantExtraOutputs texinfo;
    # nativeBuildInputs = lib.optional nativeComp gcc;

    # If the repository contains a Makefile, configurePhase can be problematic, so
    # exclude it.
    phases = ["unpackPhase" "patchPhase" "buildPhase" "checkPhase" "installPhase"];

    patches = [];

    # False by default; You can override this later
    doCheck = false;

    renamePhase = lib.optionalString (attrs ? renames && attrs.renames != null) (
      lib.pipe attrs.renames [
        (lib.mapAttrsToList (
          origin: dest: "mv ${
            if lib.hasSuffix "/" origin
            then origin + "*.*"
            else lib.removeSuffix "/" origin
          } ${
            if dest == ""
            then "."
            else dest
          }"
        ))
        (concatStringsSep "\n")
      ]
    );

    preBuild = attrs.preBuild or "";

    setSourceRoot =
      if attrs.doTangle
      then ''
        mkdir build
        ${copySourceCommand}
        sourceRoot="$PWD/build"
      ''
      else "";

    buildPhase = ''
      export EMACSLOADPATH
      runHook renamePhase

      runHook preBuild
      runHook buildCmd
      runHook postBuild

      if [[ " ''${outputs[*]} " = *" info "* ]]
      then
        runHook buildInfo
      fi
    '';

    buildInfo = ''
      cwd="$PWD"
      cd $src
      for d in $(find -name '*.texi' -o -name '*.texinfo')
      do
        local basename=$(basename $d)
        local i=$cwd/''${basename%%.*}.info
        if [[ ! -e "$i" ]]
        then
          cd $src/$(dirname $d)
          makeinfo --no-split "$basename" -o "$i"
        fi
      done
      cd $cwd
    '';

    EMACSLOADPATH =
      lib.concatStrings
      (map (pkg: "${pkg.outPath}/share/emacs/site-lisp/:")
        elispInputs);

    EMACSNATIVELOADPATH = "${
      lib.makeSearchPath "share/emacs/native-lisp/" elispInputs
    }:";

    errorOnWarn = false;
    inherit dontByteCompile;

    buildCmd = ''
      # Don't make the package description of package.el available
      rm -f *-pkg.el

      if [[ ! -n "$dontByteCompile" ]]
      then
        (
          if [[ -n "$errorOnWarn" ]]
          then
            byte_compile_error_on_warn=t
          else
            byte_compile_error_on_warn=nil
          fi
          # TODO: Add support for byte-compiling files separately
          emacs --batch -L . \
            --eval "(setq byte-compile-error-on-warn ''${byte_compile_error_on_warn})" \
            -f batch-byte-compile ${lib.escapeShellArgs (map stringBaseName lispFiles)}
        )
      fi

      rm -f "${ename}-autoloads.el"
      emacs --batch -l autoload \
          --eval "(setq backup-inhibited t)" \
          --eval "(setq version-control 'never)" \
          --eval "(setq generated-autoload-file \"$PWD/${ename}-autoloads.el\")" \
          -f batch-update-autoloads .
    '';

    # Because eln depends on the file name hash of the source file, native
    # compilation must be done after the elisp files are installed. For details,
    # see the documentation of comp-el-to-eln-rel-filename.
    buildAndInstallNativeLisp = ''
      nativeLispDir=$out/share/emacs/native-lisp
      mkdir -p $nativeLispDir

      EMACSLOADPATH="$EMACSLOADPATH" EMACSNATIVELOADPATH="$EMACSNATIVELOADPATH" \
        emacs --batch -L $lispDir -l ${compNative} \
          --eval "(push \"$nativeLispDir/\" native-comp-eln-load-path)" \
          --eval "(setq native-compile-target-directory \"$nativeLispDir/\")" \
          -f run-native-compile-sync $lispDir
    '';

    doNativeComp = nativeComp && nativeCompileAhead;

    installPhase = ''
      runHook preInstall

      lispDir=$out/share/emacs/site-lisp/
      install -d $lispDir
      tar cf - \
        --exclude='*.info' \
        --exclude='*.texi' \
        --exclude='*.texinfo' \
        --exclude='eln-cache' \
        . \
        | (cd $lispDir && tar xf -)

      if ! [[ -n "$dontByteCompile" ]] && [[ -n "$doNativeComp" ]]
      then
        runHook buildAndInstallNativeLisp
      fi

      if [[ " ''${outputs[*]} " = *" info "* ]]
      then
        runHook installInfo
      fi

      runHook postInstall
    '';

    installInfo = ''
      mkdir -p $info/share
      install -d $info/share/info
      for i in ${ename}*.info
      do
        install -t $info/share/info $i
      done
    '';
  }
