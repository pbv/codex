<apply template="base">
<h1>Bem-vindo!</h1>

<p><em>Codex</em> é um sistema <em>web</em> para 
ensino que permite
resolução de exercícios de programação 
com correção automática.</p>
<p>Atualmente o <em>Codex</em> suporta as linguagens
 <a href="http://www.python.org">Python</a>,
 <a href="http://www.haskell.org">Haskell</a> e 
 <a href="https://en.wikipedia.org/wiki/C_(programming_language)">C</a>.
</p>

<p>Por favor autentique-se para iniciar a sessão
 ou  <a href="/register">registe</a>  uma nova conta de utilizador.</p>

<form method="POST" action="/login">
  <table>
    <tr>
      <td>Login:</td><td><input type="text" name="login" size="20" /></td>
    </tr>
    <tr>
      <td>Password:</td><td><input type="password" name="password" size="20" /></td>
    </tr>
    <tr>
      <td></td>
      <td><input type="submit" value="Iniciar sessão" /></td>
    </tr>
  </table>
</form>

<div class="warnings"><p><loginError/></p></div>
</apply>
