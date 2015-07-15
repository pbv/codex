<apply template="base">
<h1>Bem-vindo!</h1>

<p><em>Pythondo</em> é um sistema <em>web</em> para resolução
 de problemas de programação na 
 <a href="http://www.python.org">linguagem Python.</a>
</p>
<p>Por favor autentique-se para iniciar uma sessão.</p>

<bind tag="post_action">/login</bind>
<bind tag="submit_label">Login</bind>
<apply template="userform"/>
<div class="warnings"><p><loginError/></p></div>
</apply>
