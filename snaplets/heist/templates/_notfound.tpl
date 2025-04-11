
<apply template="_base">
  <h1>Page not found</h1>

  <ul>
    <li>The page you requested could not be found.</li>
    <li><ifLoggedIn>
	Go back to the <a href="${home}">home page.</a>
      </ifLoggedIn>
      <ifLoggedOut>
	Go back to the <a href="${login}">authentication page.</a>
      </ifLoggedOut>
    </li>
</ul>
</apply>

<apply template="_browse"/>
<apply template="_footer"/>
