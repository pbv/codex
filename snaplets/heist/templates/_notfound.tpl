<apply template="base">
<h1>Página não existente</h1>
<ul>
<li>A página que tentou aceder não existe;</li>
<li><ifLoggedIn>
   Voltar à <a href="/">página inicial</a>
    </ifLoggedIn>
    <ifLoggedOut>
    Voltar à <a href="/login">página de autenticação.</a>
    </ifLoggedOut>
</li>
</ul>
</apply>
