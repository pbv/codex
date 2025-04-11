<apply template="_base">
<h1>Internal server error</h1>
<ul>
<li>An internal server error has happened; this should not have
occurred.</li>
<ifLoggedIn>
<li><pre><errorMsg/></pre></li>
</ifLoggedIn>
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
