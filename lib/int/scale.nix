val: percent: let
  # Parse the decimal percentage (e.g., 0.8)
  decimal = builtins.fromJSON (toString percent);

  # Calculate the result as a float
  result = val * decimal;

  # Get the integer part
  intPart = builtins.floor result;

  # Round up if there's any fractional part
  rounded =
    if result > intPart
    then intPart + 1
    else intPart;
in
  rounded
