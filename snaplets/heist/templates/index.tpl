<html>
    <head>
       <meta charset="UTF-8">
       <ifLoggedIn>
       <meta http-equiv="refresh" content="1;url=/problems"/>
       <script type="text/javascript">
            window.location.href = "/problems"
        </script>
       </ifLoggedIn>
       <ifLoggedOut>
       <meta http-equiv="refresh" content="1;url=/login"/>
       <title>Redirecionamento</title>
       </ifLoggedOut>
    </head>
    <body>
        <p>Se não for redirecionado automaticamente, siga a ligação para a
      <ifLoggedIn>
	<a href="/problems">lista de problemas.</a>
      </ifLoggedIn>
      <ifLoggedOut>
	<a href="/login">autenticação.</a>
      </ifLoggedOut>
      </p>
    </body>
</html>
