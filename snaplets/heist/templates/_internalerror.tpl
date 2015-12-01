<apply template="base">
<h1>Erro interno do servidor</h1>
<ul>
<li>Aconteceu um erro interno do servidor; isto não deveria ter acontecido!</li>
<li>Segue-se a mensagem de erro detalhada:
<pre>
<errorMsg/>
</pre>
</li>
<li><ifLoggedIn>
   Voltar à <a href="/">página inicial</a>
    </ifLoggedIn>
    <ifLoggedOut>
    Voltar à <a href="/login">página de autenticação.</a>
    </ifLoggedOut>
</li>
</ul>
</apply>
