<apply template="base">
<h1>Pedido inválido</h1>
<ul>
<li>O pedido que tentou efetuar é inválido;</li>
<li><ifLoggedIn>
   Voltar à <a href="/problems">lista de problemas</a>
    </ifLoggedIn>
    <ifLoggedOut>
    Voltar à <a href="/login">página de autenticação.</a>
    </ifLoggedOut>
</li>
</ul>
</apply>