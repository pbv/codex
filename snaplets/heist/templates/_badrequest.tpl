<apply template="_base">
<h1>Invalid request</h1>
<ul>
<li>Something unexpected happened!</li>
<li><ifLoggedIn>	 
    Go back to the <a href="${home}">home page.	</a>
  </ifLoggedIn> 
  <ifLoggedOut>	
    Go to the <a href="${login}">authenticatinn page.</a>
  </ifLoggedOut>	
</li>
</ul>
</apply>

<apply template="_browse"/>
<apply template="_footer"/>
