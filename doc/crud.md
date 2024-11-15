CRUD SQL for Blog

November 8, 2024
This conversation may reflect the link creator’s personalized data, which isn’t shared and can meaningfully change how the model responds.
You said:
Give examples of several different kinds of CRUD SQL queries one would have for a simple blog might have. Doesn't have to be anything too complicated, but including a few joins and table schemas would be great. Think about how to get show off various different SQL features and kinds of queries that would be necessary to host a blog
ChatGPT

Thought for 28 seconds

Certainly! Below are examples of various CRUD (Create, Read, Update, Delete) SQL queries for a simple blog application. These examples include table schemas, joins, and demonstrate different SQL features commonly used when hosting a blog.

Table Schemas
1. Users Table
CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    username VARCHAR(50) UNIQUE NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    password_hash VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
2. Posts Table
CREATE TABLE posts (
    post_id SERIAL PRIMARY KEY,
    user_id INTEGER NOT NULL,
    title VARCHAR(200) NOT NULL,
    content TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);
3. Comments Table
CREATE TABLE comments (
    comment_id SERIAL PRIMARY KEY,
    post_id INTEGER NOT NULL,
    user_id INTEGER NOT NULL,
    content TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    FOREIGN KEY (post_id) REFERENCES posts(post_id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(user_id) ON DELETE CASCADE
);
4. Categories Table
CREATE TABLE categories (
    category_id SERIAL PRIMARY KEY,
    name VARCHAR(100) UNIQUE NOT NULL
);
5. Post_Categories Table (Many-to-Many Relationship)
CREATE TABLE post_categories (
    post_id INTEGER NOT NULL,
    category_id INTEGER NOT NULL,
    PRIMARY KEY (post_id, category_id),
    FOREIGN KEY (post_id) REFERENCES posts(post_id) ON DELETE CASCADE,
    FOREIGN KEY (category_id) REFERENCES categories(category_id) ON DELETE CASCADE
);
CRUD Operations
Create Operations (INSERT)
1. Create a New User
INSERT INTO users (username, email, password_hash)
VALUES ('john_doe', 'john@example.com', 'hashed_password');
2. Create a New Post
INSERT INTO posts (user_id, title, content)
VALUES (1, 'My First Blog Post', 'This is the content of my first post.');
3. Create a New Category
INSERT INTO categories (name)
VALUES ('Technology');
4. Associate a Post with a Category
INSERT INTO post_categories (post_id, category_id)
VALUES (1, 1);
5. Add a Comment to a Post
INSERT INTO comments (post_id, user_id, content)
VALUES (1, 2, 'Great post!');
Read Operations (SELECT)
1. Get All Posts with Their Authors
SELECT
    p.post_id,
    p.title,
    p.content,
    u.username AS author,
    p.created_at
FROM
    posts p
JOIN
    users u ON p.user_id = u.user_id;
2. Get All Comments for a Specific Post
SELECT
    c.comment_id,
    c.content,
    u.username AS commenter,
    c.created_at
FROM
    comments c
JOIN
    users u ON c.user_id = u.user_id
WHERE
    c.post_id = 1
ORDER BY
    c.created_at ASC;
3. Get All Categories for a Specific Post
SELECT
    cat.name
FROM
    categories cat
JOIN
    post_categories pc ON cat.category_id = pc.category_id
WHERE
    pc.post_id = 1;
4. Get All Posts in a Specific Category
SELECT
    p.post_id,
    p.title,
    p.content
FROM
    posts p
JOIN
    post_categories pc ON p.post_id = pc.post_id
JOIN
    categories c ON pc.category_id = c.category_id
WHERE
    c.name = 'Technology';
5. Get the Number of Comments per Post
SELECT
    p.post_id,
    p.title,
    COUNT(c.comment_id) AS comment_count
FROM
    posts p
LEFT JOIN
    comments c ON p.post_id = c.post_id
GROUP BY
    p.post_id, p.title;
6. Get the Most Recent Posts
SELECT
    p.post_id,
    p.title,
    p.created_at
FROM
    posts p
ORDER BY
    p.created_at DESC
LIMIT 5;
7. Get Users Who Have Never Posted
Using a Subquery:

SELECT
    u.username
FROM
    users u
WHERE
    u.user_id NOT IN (SELECT p.user_id FROM posts p);
Using a LEFT JOIN:

SELECT
    u.username
FROM
    users u
LEFT JOIN
    posts p ON u.user_id = p.user_id
WHERE
    p.post_id IS NULL;
Update Operations (UPDATE)
1. Update a User's Email
UPDATE
    users
SET
    email = 'new_email@example.com'
WHERE
    user_id = 1;
2. Update a Post's Content
UPDATE
    posts
SET
    content = 'Updated content of the post.',
    updated_at = CURRENT_TIMESTAMP
WHERE
    post_id = 1;
3. Change a Category Name
UPDATE
    categories
SET
    name = 'Tech'
WHERE
    category_id = 1;
Delete Operations (DELETE)
1. Delete a Comment
DELETE FROM
    comments
WHERE
    comment_id = 1;
2. Delete a Post (and Cascade Delete Comments)
DELETE FROM
    posts
WHERE
    post_id = 1;
Note: The ON DELETE CASCADE constraint on foreign keys ensures related comments are deleted automatically.

3. Remove a Category Association from a Post
DELETE FROM
    post_categories
WHERE
    post_id = 1 AND category_id = 1;
Advanced Queries and Features
1. Get Posts with Comment Counts and Categories
Using STRING_AGG (PostgreSQL) or GROUP_CONCAT (MySQL) to Aggregate Categories:

PostgreSQL:

SELECT
    p.post_id,
    p.title,
    u.username AS author,
    COUNT(c.comment_id) AS comment_count,
    STRING_AGG(DISTINCT cat.name, ', ') AS categories
FROM
    posts p
JOIN
    users u ON p.user_id = u.user_id
LEFT JOIN
    comments c ON p.post_id = c.post_id
LEFT JOIN
    post_categories pc ON p.post_id = pc.post_id
LEFT JOIN
    categories cat ON pc.category_id = cat.category_id
GROUP BY
    p.post_id, p.title, u.username;
MySQL:

SELECT
    p.post_id,
    p.title,
    u.username AS author,
    COUNT(c.comment_id) AS comment_count,
    GROUP_CONCAT(DISTINCT cat.name) AS categories
FROM
    posts p
JOIN
    users u ON p.user_id = u.user_id
LEFT JOIN
    comments c ON p.post_id = c.post_id
LEFT JOIN
    post_categories pc ON p.post_id = pc.post_id
LEFT JOIN
    categories cat ON pc.category_id = cat.category_id
GROUP BY
    p.post_id, p.title, u.username;
2. Using Transactions
Deleting a User and All Related Data Atomically:

BEGIN TRANSACTION;

DELETE FROM comments WHERE user_id = 1;
DELETE FROM posts WHERE user_id = 1;
DELETE FROM users WHERE user_id = 1;

COMMIT;
Alternatively, with ON DELETE CASCADE, deleting the user will automatically delete related posts and comments.

3. Adding Constraints
Ensuring Post Content Has a Minimum Length:

ALTER TABLE
    posts
ADD CONSTRAINT
    content_length_check CHECK (LENGTH(content) >= 10);
4. Creating a View
Creating a View for Posts with Comment Counts:

CREATE VIEW posts_with_comment_counts AS
SELECT
    p.post_id,
    p.title,
    p.user_id,
    COUNT(c.comment_id) AS comment_count
FROM
    posts p
LEFT JOIN
    comments c ON p.post_id = c.post_id
GROUP BY
    p.post_id, p.title, p.user_id;
Querying the View:

SELECT
    *
FROM
    posts_with_comment_counts
WHERE
    comment_count > 0;
Explanation of SQL Features Used
Joins: Used to combine rows from two or more tables based on related columns (JOIN, LEFT JOIN).
Aggregate Functions: Functions like COUNT, STRING_AGG, and GROUP_CONCAT to perform calculations on multiple rows.
Subqueries: Nested queries used within another SQL query to further filter results.
Transactions: Ensures a sequence of operations are executed as a single unit (BEGIN TRANSACTION, COMMIT).
Constraints: Rules enforced on data columns (PRIMARY KEY, FOREIGN KEY, UNIQUE, CHECK).
Views: Virtual tables representing the result of a database query (CREATE VIEW).
These examples cover various SQL operations and features that are essential for managing a simple blog application. They demonstrate how to create and manipulate data across multiple related tables, ensuring data integrity and efficient data retrieval.
