<html>
    <head>
       <meta charset="UTF-8">
       <ifLoggedIn>
       <meta http-equiv="refresh" content="1;url=${home}"/>
       </ifLoggedIn>
       <ifLoggedOut>
       <meta http-equiv="refresh" content="1;url=${login}"/>
       <title>Redirecionamento</title>
       </ifLoggedOut>
    </head>
    <body>
        <p>Se não for redirecionado automaticamente, siga a ligação para a
      <ifLoggedIn>
	<a href="${home}">página inicial.</a>
      </ifLoggedIn>
      <ifLoggedOut>
	<a href="${login}">autenticação.</a>
      </ifLoggedOut>
      </p>
    </body>
</html>
