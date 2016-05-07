<apply template="base">
<h1>Welcome!</h1>

<p><em>Codex</em> is a <em>web</em> system for
setting programming exercises
with automatic assement 
for teaching programming
in a class environment.</p>
<p>Currently <em>Codex</em> supports 
 <a href="http://www.python.org">Python</a>,
 <a href="http://www.haskell.org">Haskell</a> and
the <a href="https://en.wikipedia.org/wiki/C_(programming_language)">C</a>
programming languages.
</p>
<p>Please log in to begin your session.</p>

<bind tag="post_action">/login</bind>
<bind tag="submit_label">Login</bind>
<apply template="userform"/>
<div class="warnings"><p><loginError/></p></div>
</apply>
