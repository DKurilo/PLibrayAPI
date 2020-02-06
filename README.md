# Library API 

[![Build Status](https://travis-ci.org/DKurilo/PLibrayAPI.svg?branch=master)](https://travis-ci.org/DKurilo/PLibrayAPI)  

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

## API description

### Authentication

To autheticate use header `Auth-Token`. As header value use account token.  
In case user with token not found or user with such role don't have access, 403 status will be returned with message `Invalid Token`.
In case token is awaiting, but there is no token, 401 will be returned.

### GET /api/v1/health

The way to check if service still alive.

### Access

Public. Authentication is not required.

#### Response

- Status code 200
- Current server timestamp

#### Example

```
> curl -X GET "https://127.0.0.1:8080/api/v1/health" -H "Content-Type: application/json" -k  
"1581011209940"
```

### POST /api/v1/librarian/books

Add book to library.  

#### Access

User with role `librarian`.

#### Request parameters

Proper ISBN. For example

```
"0-306-40615-2"
```

#### Response

- Status code 201
- JSON with created record
or  
- Status code 400
- Wrong ISBN

#### Examples

```
curl -X POST "https://127.0.0.1:8080/api/v1/librarian/books" -H "Content-Type: application/json" -H "Auth-Token: 1" --data-binary '"0-306-40615-2"' -k
{"bookId":9,"isbn":"0-306-40615-2"}
```
```
curl -X POST "https://127.0.0.1:8080/api/v1/librarian/books" -H "Content-Type: application/json" -H "Auth-Token: 1" --data-binary '"1-306-40615-2"' -k
Wrong ISBN
```

### DELETE /api/v1/librarian/books

Remove book from library.  

#### Access

User with role `librarian`.

#### Request parameters

Proper ISBN, that is in library and not checked out. For example

```
"0-306-40615-2"
```

#### Response

- Status code 202
- Removed book_id
or  
- Status code 400
- Wrong ISBN
or  
- Status code 404
- Can't delete book

#### Examples

```
curl -X DELETE "https://127.0.0.1:8080/api/v1/librarian/books" -H "Content-Type: application/json" -H "Auth-Token: 1" --data-binary '"0-306-40615-2"' -k
2
```
```
curl -X DELETE "https://127.0.0.1:8080/api/v1/librarian/books" -H "Content-Type: application/json" -H "Auth-Token: 1" --data-binary '"1-306-40615-2"' -k
Wrong ISBN
```
```
curl -X DELETE "https://127.0.0.1:8080/api/v1/librarian/books" -H "Content-Type: application/json" -H "Auth-Token: 1" --data-binary '"2-306-40615-4"' -k
Can't delete book
```

### Get /api/v1/librarian/books/overdue

Return list of overdued books.

#### Access

User with role `librarian`.

#### Response

- Status code 200
- List of overdued books

#### Example

```
curl -X GET "https://127.0.0.1:8080/api/v1/librarian/books/overdue" -H "Content-Type: application/json" -H "Auth-Token: 1" -k
[{"daysOverdue":"16","isbn":"0-306-40615-2","user":"3","bookId":"8"}]
```

### GET /api/v1/user/books

Return list of books, that are checked out by this user.

#### Access

User with role `user`.  

#### Response

- Status code 200
- List of books that are checked out by user

#### Examples

```
curl -X GET "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" -k
[{"checkoutDate":"2020-02-06 00:48:32.554787","daysLeft":"14","endDate":"2020-02-20 00:48:32.554787","isbn":"1-306-40615-3","bookId":"3"},{"checkoutDate":"2020-02-06 00:46:49.451523","daysLeft":"14","endDate":"2020-02-20 00:46:49.451523","isbn":"0-306-40615-2","bookId":"5"}]
```
```
curl -X GET "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 3" -k
[{"checkoutDate":"2020-01-07 13:16:29.283356","daysLeft":"-16","endDate":"2020-01-21 13:16:29.283356","isbn":"0-306-40615-2","bookId":"8"}]
```

### POST /api/v1/user/books

Check out a book.

#### Access

User with role `user`.

#### Request parameters

Proper ISBN. For example

```
"0-306-40615-2"
```

#### Response

- Status code 201
- JSON with created record
or  
- Status code 400
- Wrong ISBN
or  
- Status code 404
- There is no such book currently in library
or  
- Status code 412
- You have overdued book
or  
- Status code 412
- You have too many checked out books

#### Examples

```
curl -X POST "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" --data-binary '"0-306-40615-2"' -k
{"checkoutDate":"2020-02-06 13:37:29.352481","daysLeft":"14","endDate":"2020-02-20 13:37:29.352481","isbn":"0-306-40615-2","bookId":"9"}
```
```
curl -X POST "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" --data-binary '"0-306-40615-2"' -k
You have too many checked out books
```
```
curl -X POST "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 3" --data-binary '"0-306-40615-2"' -k
You have overdued book
```

### DELETE /api/v1/user/books

Return book to library.

#### Access

User with role `user`.

#### Request parameters

Proper ISBN, that is in library and checked out by this user. For example

```
"0-306-40615-2"
```

#### Response

- Status code 202
- Removed book_id
or  
- Status code 400
- Wrong ISBN
or  
- Status code 404
- Can't return book

#### Examples

```
curl -X DELETE "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" --data-binary '"0-306-40615-2"' -k
2
```
```
curl -X DELETE "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" --data-binary '"1-306-40615-2"' -k
Wrong ISBN
```
```
curl -X DELETE "https://127.0.0.1:8080/api/v1/user/books" -H "Content-Type: application/json" -H "Auth-Token: 2" --data-binary '"2-306-40615-4"' -k
Can't return book
```

## CI/CD

Project uses travis-ci.org to make build project, make tests and push docker image into docker hub.  

## How to run

### Database

Install postgersql (or use postgresql docker image).  
Initialize database with scripts from `db/`.  
`empty-init.sql` creates databse library and user library without password. You can change it.  
`empty-dump.sql` creates tables and users in database.  

### With Docker

```
docker pull dkurilo/library-api
docker run -t -i -d -e POSTGRES_CONN="postgresql://library@host.docker.internal/library" -p 8080:8080 dkurilo/library-api
```

### Build locally from sorce

Install stack: https://docs.haskellstack.org/en/stable/README/  
Build project with  

```
stack build
```

Start project

```
POSTGRES_CONN="postgresql://library@localhost/library" stack run library-api-exe
```

To run test

- build database form `db/test-init.sql` and `db/test-data.sql`  
- run tests with
```
POSTGRES_CONN=postgresql://librarytest@localhost/librarytest stack test
```
