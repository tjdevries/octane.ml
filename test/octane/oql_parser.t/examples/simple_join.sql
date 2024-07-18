SELECT User.name, Post.content
  FROM Post 
    INNER JOIN User ON User.id = Post.author
