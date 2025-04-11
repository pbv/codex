<apply template="_base">
<h1>Unauthorized</h1>

<ifLoggedIn>
  <ul>
    <li>You don't have permission to view this page.</li>
  </ul>
</ifLoggedIn>
<ifLoggedOut>
  <ul>
    <li>You are not logged in.</li>
    <li>Go back to the <a href="${login}">login page</a>
      to authenticate.</li>
  </ul>
</ifLoggedOut>
<apply template="_browse"/>
<apply template="_footer"/>
</apply>


