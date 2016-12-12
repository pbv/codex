<apply template="base">
<h1>Submissions list</h1>

<table>
  <form action="/submit" method="POST">
    <tr>
      <th>id</th>
      <th>user_id</th>
      <th>path</th>
      <th>time</th>
      <th>language</th>
      <th>class</th>
      <th>timing</th>
    </tr>
    <tr>
      <td><input type="text" name="id_pat" size="5"/></td>
      <td><input type="text" name="uid_pat" size="8"/></td>
      <td><input type="text" name="path_pat" size="20"/></td>
      <td><input type="text" name="time_pat" size="30"/></td>
      <td><input type="text" name="lang_pat" size="8"/></td>
      <td><input type="text" name="class_pat" size="20"/></td>
      <td><input type="text" name="timing_pat" size="6"/></td>
      <td><input type="submit" value="Filter"/></td>
    </tr>
  </form>
  <submissions>
    <tr>
      <td><a href="/submit/${submit-id}"><submit-id/></a></td>
      <td><submit-user-id/></td>
      <td><submit-path/></td>
      <td><time/></td>
      <td><code-lang/></td>
      <td><classify/></td>
      <td><timing/></td>
    </tr>
  </submissions>
</table>

<if-submissions>
  <p>Submissions count: <submissions-count/></p>
</if-submissions>
</apply>
