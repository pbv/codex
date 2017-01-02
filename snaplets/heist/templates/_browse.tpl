<div id="actions">
  <ifLoggedOut>
  <em>Codex <version/>. <timeNow/></em>
  </ifLoggedOut>
  <ifLoggedIn>
    <ul class="menubar">
       <li><a href="/pub"
        title="Voltar ao ínício">Início</a></li>
    <ifAdmin>
       <li><a href="/files"
	 title="Ver ficheiros">Ficheiros</a></li>
      <li><a href="/submited"
	 title="Ver submissões">Submissões</a></li>
      <apply-content/>
    </ifAdmin>
   </ul>
    <a class="button info" href="/logout" 
       style="float:right;margin-left:1em">Terminar sessão</a>
    <span class="info" style="float:right">
      <timeNow/>&nbsp;<loggedInName/>&nbsp;(<code><loggedInUser/></code>)
    </span>
  </ifLoggedIn>
</div>
