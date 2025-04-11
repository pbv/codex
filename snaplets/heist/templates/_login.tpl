<apply template="_base">

<h1>Welcome!</h1>

<p><em>Codex</em> is a <em>web</em> system for programming exercises
 with automatic assessment.
</p>

<p>Please authenticate yourself to start a session.</p>

<form method="POST" action="${login}">
  <table>
    <tr>
      <td>Login:</td><td><input type="text" name="login" size="20" /></td>
    </tr>
    <tr>
      <td>Password:</td><td><input type="password" name="password" size="20" /></td>
    </tr>
    <tr>
      <td></td>
      <td><input type="submit" value="Begin session" /></td>
    </tr>
  </table>
</form>

<div class="errors"><p><loginError/></p></div>
</apply>

<apply template="_browse"/>
<apply template="_footer"/>
