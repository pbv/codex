<apply template="_base">
<h1>Erro interno do servidor</h1>
<ul>
<li>Aconteceu um erro interno do servidor; isto não deveria ter acontecido!</li>
<ifLoggedIn>
<li><pre><errorMsg/></pre></li>
</ifLoggedIn>
<li><ifLoggedIn>
   Voltar à <a href="${home}">página inicial</a>
    </ifLoggedIn>
    <ifLoggedOut>
    Voltar à <a href="${login}">página de autenticação.</a>
    </ifLoggedOut>
</li>
</ul>
</apply>

<apply template="_browse"/>
<apply template="_footer"/>
