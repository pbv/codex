<apply template="base">
<h1>Submissions</h1>
<p> 
</p>

<form id="listform" action="/submited" method="POST">
  <p>
    <input type="button" onClick="move(-1)" value="Previous page"/> &nbsp;
    <input type="button" onClick="move(1)" value="Next page"/> 
    &nbsp;&nbsp;
    <span class="info">
      <submissions-count/> submissions (page <page/> of <page-count/>)
    </span>
  </p>
  <table class="submissions">
    <tr>
      <th/>
      <th>id</th>
      <th>user_id</th>
      <th>path</th>
      <th>language</th>
      <th>classify</th>
      <th>timing</th>
      <th>received</th>
    </tr>
    <tr>
      <th><input type="submit" value="Filter"/></th>
      <th><input type="text" name="id_pat" size="4" value="${id_pat}"/></th>
      <th><input type="text" name="uid_pat" size="8" value="${uid_pat}"/></th>
      <th><input type="text" name="path_pat" size="20" value="${path_pat}"/></th>
      <th><input type="text" name="lang_pat" size="8" value="${lang_pat}"/></th>
      <th><input type="text" name="class_pat" size="20" value="${class_pat}"/></th>
      <th><input type="text" name="timing_pat" size="6" value="${timing_pat}"/></th>
    </tr> 
    <if-submissions>
      <submissions>
	<tr>
	  <td/>
	  <td><a href="/submited/${submit-id}"><submit-id/></a></td>
	  <td><submit-user-id/></td>
	  <td><submit-path/></td>
	  <td><code-lang/></td>
	  <td><classify/></td>
	  <td><timing/></td>
	  <td><received/></td>
	</tr>
      </submissions>
      <else/>
      <tr><td colspan=7 align="center">(No submissions)</td></tr>
    </if-submissions>
  </table>
  <input id="page" type="hidden" name="page" value="${page}"/>
</form>
<script>
function move(k) {
  var form = document.getElementById("listform");
  var param = document.getElementById("page");
  var page = parseInt(param.value, 10);
  param.value = String(page + k);
  form.submit();
}
</script>

<hr/>
<form action="/export" method="POST">
  <fieldset>
    <legend>Export to text</legend>
  <p>Field separator:
    <select name="sep">
      <option value=",">comma (,)</option>
      <option value=";">semicolon (;)</option>
      <option value="&#9">tabulation</option>
    </select>
    &nbsp; <input type="submit" value="Export"/> 
  </p>
  </fieldset>
</form>

</apply>
