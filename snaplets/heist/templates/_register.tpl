<apply template="_base">
<h1>Registar novo utilizador</h1>

<form method="POST" action="${register}">
  <table>
    <tr>
      <td>Nome completo:</td> <td><input type="text" name="fullname" size=40/></td>
    </tr>
    <tr>
      <td>Email:</td> <td><input type="text" name="email" size=40/></td>
    </tr>
    <tr>
      <td>Login:</td><td><input type="text" name="login" size=20/></td>
    </tr>
    <tr>
      <td>Password:</td><td><input type="password" name="password" size="20" /></td>
    </tr>
    <tr>
      <td>Confirmar password:</td><td><input type="password" name="password2" size="20" /></td>
    </tr>
 
    <tr>
      <td/>
      <td><input type="submit" value="Registar"/> &nbsp;
	<a href="/">Cancelar</a>
      </td>
    </tr>
  </table>
</form>
<div class="warnings"><p><loginError/></p></div>
</apply>

<apply template="_browse">
</apply>
