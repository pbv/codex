<html>
    <head>
       <meta charset="UTF-8">
       <ifLoggedIn>
       <meta http-equiv="refresh" content="1;url=${home}"/>
       </ifLoggedIn>
       <ifLoggedOut>
       <meta http-equiv="refresh" content="1;url=${login}"/>
       <title>Redirecting</title>
       </ifLoggedOut>
    </head>
    <body>
        <p>If you are not automatically redirected, please follow the link to the
      <ifLoggedIn>
	<a href="${home}">home page.</a>
      </ifLoggedIn>
      <ifLoggedOut>
	<a href="${login}">authentication page.</a>
      </ifLoggedOut>
      </p>
    </body>
</html>
