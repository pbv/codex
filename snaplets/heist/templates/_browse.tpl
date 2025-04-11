<div id="actions">
  <ifLoggedOut>
    <em>Codex <version/>. <timeNow/></em>
  </ifLoggedOut>
  <ifLoggedIn>
    <ul class="menubar">
       <li><a href="${home}" 
              title="Go to the home page"><apply template="_icon_home"/></a></li>
       <ifAdmin>
	 <li><a class="admin" href="${admin}"
		title="Admin operations"><apply template="_icon_admin"/></a></li>
       </ifAdmin>
       <apply-content/>
    </ul>
    <span id="uppercorner">
      <timeNow/>&nbsp;<loggedInName/>&nbsp;(<code><loggedInUser/></code>)
      &nbsp;<a class="button" href="${logout}">Logout</a> 
    </span>
  </ifLoggedIn>
</div>
