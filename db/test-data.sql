CREATE TABLE book(book_id serial PRIMARY KEY, ISBN VARCHAR (30) NOT NULL);
CREATE TABLE account(user_id int PRIMARY KEY, role VARCHAR (10) NOT NULL, token VARCHAR (50) NOT NULL);
CREATE TABLE account_book(user_id int NOT NULL, book_id integer NOT NULL, check_date timestamp without time zone, end_date timestamp without time zone, PRIMARY KEY (user_id, book_id), CONSTRAINT account_book_user_id_fkey FOREIGN KEY (user_id) REFERENCES account (user_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION, CONSTRAINT account_book_book_id_fkey FOREIGN KEY (book_id) REFERENCES book (book_id) MATCH SIMPLE ON UPDATE NO ACTION ON DELETE NO ACTION);
INSERT INTO account (user_id, role, token) VALUES (1, 'librarian', '1'), (2, 'user', '2'), (3, 'user', '3'), (4, 'user', '4');
INSERT INTO book (ISBN) VALUES ('9780441013593'), ('9780738811512'), ('9780316229296');
INSERT INTO account_book (user_id, book_id, check_date, end_date) VALUES (2, 1, now() - interval '30 days', now() - interval '16 days'), (4, 2, now() - interval '20 days', now() - interval '6 days');
