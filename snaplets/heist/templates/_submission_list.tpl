<apply template="_base">
  <apply template="_browse"/>
<script>
function reevaluate() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "PATCH";
  form.submit();
}
function cancel() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "CANCEL";
  form.submit();
}
function listsubmissions() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "EXPORT";
  form.submit();
}
function printsubmissions() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "PRINT";
  form.submit();
}
</script>
<h1>Submissions</h1>
<form id="listform" action="${submissionList}" method="POST">
  <table class="submissions">
    <tr>
      <td colspan="6">
	<span class="info">
	  <submissions-count/> submissions (page <page/> of <page-count/>)
	</span> &nbsp;
	<select name="order" onChange="this.form.submit()">
	  <if-ascending>
	    <option value="Ascending" selected>ascending</option>
	    <option value="Descending">descending</option>
	    <else/>
	    <option value="Ascending">ascending</option>
	    <option value="Descending" selected>descending</option>
	  </if-ascending>
	</select> &nbsp;
	<a class="button" href="${submissions-prev-url}">&lt;</a> &nbsp;
	<a class="button" href="${submissions-next-url}">&gt;</a> &nbsp;
	<input type="submit" value="Filter"/>
      </td>
      <td colspan="1">
	<input type="button" onClick="reevaluate()" value="Re-evaluate"/>
	<input type="button" onClick="cancel()" value="Cancel"/>	
      </td>
    </tr>
    <tr>
      <th>id</th>
      <th>user_id</th>
      <th>path</th>
      <th>language</th>
      <th>status</th>
      <th>policy</th>
      <th>received</th>
    </tr>
    <tr>
      <th><input type="text" name="id" size="4" value="${id}"/></th>
      <th><input type="text" name="user_id" size="8" value="${user_id}"/></th>
      <th><input type="text" name="path" size="20" value="${path}"/></th>
      <th><input type="text" name="language" size="8" value="${language}"/></th>
      <th><input type="text" name="status" size="20" value="${status}"/></th>
      <th><input type="text" name="policy" size="20" value="${policy}"/></th>
      <th/>
    </tr>
    <if-submissions>
      <submissions>
	<tr>
	  <td class="submitid"><a href="${submission-admin-url}"><submit-id/></a></td>
	  <td class="userid"><submit-user-id/></td>
	  <td class="path"><a href="${file-url}"><submit-path/></a></td>
	  <td class="lang"><submit-lang/></td>
	  <td class="status ${result-status}"><result-status/></td>
	  <td class="check"><result-check/></td>
	  <td class="received"><submit-time/></td>
	</tr>
      </submissions>
      <else/>
      <tr><td colspan="7" align="center">(No submissions)</td></tr>
    </if-submissions>
  </table>
<hr/>
 <input type="button" onClick="listsubmissions()" title="Export as text" value="Export"/> separator:
 <select name="sep">
    <option value=",">comma (,)</option>
    <option value=";">semicolon (;)</option>
    <option value="&#9">tab</option>
  </select>
&emsp;<input type="button" onClick="printsubmissions()" title="Generate printouts" value="Generate printouts"/>
<hr/>
<input id="page" type="hidden" name="page" value="${page}"/>
<input id="_method" type="hidden" name="_method" value="GET"/>
</form>
</apply>

