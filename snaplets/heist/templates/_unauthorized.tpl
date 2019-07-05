
<apply template="_base">
<h1>Acesso não autorizado!</h1>

<ifLoggedIn>
  <ul>
    <li>Não tem permissões para visualizar esta página.</li>
  </ul>
</ifLoggedIn>

<ifLoggedOut>
  <ul>
    <li>Para visualizar esta página nessita de estar autenticado.</li>
    <li>Volte à <a href="${login}">página de entrada</a>
      para se autenticar.</li>
  </ul>
</ifLoggedOut>
</apply>

<apply template="_browse"/>
<apply template="_footer"/>

