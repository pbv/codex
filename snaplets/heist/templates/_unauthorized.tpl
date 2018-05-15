<apply template="_base">
<h1>Acesso não autorizado!</h1>
<ul>
<li>Para visualizar esta página nessita de estar autenticado.</li>
<ifLoggedOut>
  <li>Se a sua sessão expirou por inatividade
    tente <a href="${login}">autenticar-se novamente.</a></li>
</ifLoggedOut>
</ul>
</apply>
