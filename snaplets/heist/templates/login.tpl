<bind tag="application">Codex</bind>
<apply template="base">
<h1>Welcome!</h1>

<p><em><application/></em> is a <em>web</em> system for
setting programming problems with automatic assement
for a class environment.</p>
<p>Currently <application/> supports both the
 <a href="http://www.python.org">Python</a> and
 <a href="http://www.haskell.org">Haskell</a> programming languages.
</p>
<p>Please log in to begin your session.</p>

<bind tag="post_action">/login</bind>
<bind tag="submit_label">Login</bind>
<apply template="userform"/>
<div class="warnings"><p><loginError/></p></div>
</apply>
