--
-- PostgreSQL database dump
--

-- Dumped from database version 12.1
-- Dumped by pg_dump version 12.1

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: account; Type: TABLE; Schema: public; Owner: library
--

CREATE TABLE public.account (
    user_id integer NOT NULL,
    role character varying(10) NOT NULL,
    token character varying(50) NOT NULL
);


ALTER TABLE public.account OWNER TO library;

--
-- Name: account_book; Type: TABLE; Schema: public; Owner: library
--

CREATE TABLE public.account_book (
    user_id integer NOT NULL,
    book_id integer NOT NULL,
    check_date timestamp without time zone,
    end_date timestamp without time zone
);


ALTER TABLE public.account_book OWNER TO library;

--
-- Name: book; Type: TABLE; Schema: public; Owner: library
--

CREATE TABLE public.book (
    book_id integer NOT NULL,
    isbn character varying(30) NOT NULL
);


ALTER TABLE public.book OWNER TO library;

--
-- Name: book_book_id_seq; Type: SEQUENCE; Schema: public; Owner: library
--

CREATE SEQUENCE public.book_book_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.book_book_id_seq OWNER TO library;

--
-- Name: book_book_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: library
--

ALTER SEQUENCE public.book_book_id_seq OWNED BY public.book.book_id;


--
-- Name: book book_id; Type: DEFAULT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.book ALTER COLUMN book_id SET DEFAULT nextval('public.book_book_id_seq'::regclass);


--
-- Data for Name: account; Type: TABLE DATA; Schema: public; Owner: library
--

COPY public.account (user_id, role, token) FROM stdin;
1	librarian	1
2	user	2
3	user	3
4	user	4
\.


--
-- Data for Name: account_book; Type: TABLE DATA; Schema: public; Owner: library
--

COPY public.account_book (user_id, book_id, check_date, end_date) FROM stdin;
\.


--
-- Data for Name: book; Type: TABLE DATA; Schema: public; Owner: library
--

COPY public.book (book_id, isbn) FROM stdin;
\.


--
-- Name: book_book_id_seq; Type: SEQUENCE SET; Schema: public; Owner: library
--

SELECT pg_catalog.setval('public.book_book_id_seq', 0, true);


--
-- Name: account_book account_book_pkey; Type: CONSTRAINT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.account_book
    ADD CONSTRAINT account_book_pkey PRIMARY KEY (user_id, book_id);


--
-- Name: account account_pkey; Type: CONSTRAINT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.account
    ADD CONSTRAINT account_pkey PRIMARY KEY (user_id);


--
-- Name: book book_pkey; Type: CONSTRAINT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.book
    ADD CONSTRAINT book_pkey PRIMARY KEY (book_id);


--
-- Name: account_book account_book_book_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.account_book
    ADD CONSTRAINT account_book_book_id_fkey FOREIGN KEY (book_id) REFERENCES public.book(book_id);


--
-- Name: account_book account_book_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: library
--

ALTER TABLE ONLY public.account_book
    ADD CONSTRAINT account_book_user_id_fkey FOREIGN KEY (user_id) REFERENCES public.account(user_id);


--
-- PostgreSQL database dump complete
--

