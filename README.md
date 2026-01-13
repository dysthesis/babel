# Babel - Literature management bot

Babel is a Discord bot that fetches and manages metadata on books and papers.

The main logic is straightforward: it is designated a forum channel, and,
upon receiving a request for some literature, it

- checks the database to see if a post for it already exists,
- pulls the relevant metadata if it does not, and
- replies with the link for the post.
