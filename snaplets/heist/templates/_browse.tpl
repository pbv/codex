<div id="actions">
  <ifLoggedOut>
  <em>Codex <version/>. <timeNow/></em>
  </ifLoggedOut>
  <ifLoggedIn>
    <span class="info" style="float:right">
    <timeNow/> &nbsp;<loggedInName/> (<code><loggedInUser/></code>)
    <a class="button" href="/logout">Terminar sessão</a>
    </span>
    <ul class="menubar">
       <li><a href="/pub"
        title="Voltar ao ínício">Início</a></li>
    <ifAdmin>
       <li><a href="/files"
	 title="Browse files">Files</a></li>
      <li><a href="/submited"
	 title="Browse submissions">Submissions</a></li>
      <apply-content/>
        </ifAdmin>

   </ul>
     <span class="info" style="float:right">
    </span>
  </ifLoggedIn>
</div>
