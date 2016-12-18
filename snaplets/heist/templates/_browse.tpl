<ifLoggedIn>
  <div id="actions">
    <a href="/pub" 
       title="Voltar ao ínício">Codex <version/>.</a> &nbsp;
    <ifAdmin>
      <a class="button" href="/files" 
	 title="Browse all files">Files</a> &nbsp;
      <a class="button" href="/submited" 
	 title="View all submissions">Submissions</a> &nbsp;
      <apply-content/>
    </ifAdmin>
  </div>
</ifLoggedIn>
