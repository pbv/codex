
<div id="actions">
  <ifLoggedOut>
    <em>Codex <version/>. <timeNow/></em>
  </ifLoggedOut>
  <ifLoggedIn>
    <ul class="menubar">
       <li><a href="${home}"
              title="Voltar ao ínício">Início</a></li>
       <apply-content/>
       <ifAdmin>
	 <li><a class="admin" href="/admin"
		title="Operações de adminstração">Admin</a></li>
       </ifAdmin>
    </ul>
    <span id="uppercorner">
      <timeNow/>&nbsp;<loggedInName/>&nbsp;(<code><loggedInUser/></code>)
      &nbsp;<a class="button" href="${logout}">Logout</a> 
    </span>
  </ifLoggedIn>
</div>
