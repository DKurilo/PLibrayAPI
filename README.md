# Library API

## Task

Library API

Your task is to build a simple API to manage a library's book inventory.  There are two main components of this API, a management section (to be used by the Librarians) and a User facing API for end-user actions.

Books are referenced up by their ISBN (https://en.wikipedia.org/wiki/International_Standard_Book_Number).  The library can have more than 1 copy of any given book (multiple copies of the same ISBN).

API Endpoints to be built:

Librarian Endpoints:
* An endpoint to add a book (by ISBN) to the library.
* An endpoint to remove a book (by ISBN) from the library
* An endpoint that generates a list of all overdue books.

User Endpoints:
* An endpoint to check out a book (assume a 2 week checkout period from time of call).  A User can check out any book except when:
  - They currently have 3 checked out books.
  - They are overdue on returning any book.
* An endpoint to return a checked out book.
* An endpoint that returns all currently checked out books for that user.

For the purposes of this exercise, we can assume there is a Librarian user (userId 1)  and three regular users (userids, 2, 3, 4).  You can hardcode this table.  Also, no need to worry about authentication, etc.

## Aproach

Haskell application that use Servant to build API, warp-tls to have HTTPS and postgres as storage. It could be good idea to use Redis to store user-book and book-checked out pairs, but for now I'm going to save them in separate table.
Project should use github as VCS and travis-ci to run tests and build docker image with application that is ready to prod.

## Database structure

*account*
user_id
role
token

*book*
book_id
ISBN

*account_book*
id
user_id
book_id
check_date
term


