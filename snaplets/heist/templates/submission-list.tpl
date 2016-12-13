<apply template="base">
<bind tag="navigator">
  <p>
    <input type="submit" value="Filter"/> &nbsp;
    <input type="button" onClick="move(-1)" value="Previous page"/> &nbsp;
    <input type="button" onClick="move(1)" value="Next page"/> &nbsp;
    <submissions-count/> total (page <page/> of <page-count/>) 
  </p>
</bind>
<h1>Submissions list</h1>
<form id="listform" action="/submited" method="POST">
  <navigator/>
  <table class="submissions">
    <tr>
      <th>id</th>
      <th>user_id</th>
      <th>path</th>
      <th>language</th>
      <th>class</th>
      <th>timing</th>
      <th>time</th>
    </tr>
    <tr>
    <th><input type="text" name="id_pat" size="4" value="${id_pat}"/></th>
    <th><input type="text" name="uid_pat" size="8" value="${uid_pat}"/></th>
    <th><input type="text" name="path_pat" size="20" value="${path_pat}"/></th>
    <th><input type="text" name="lang_pat" size="8" value="${lang_pat}"/></th>
    <th><input type="text" name="class_pat" size="20" value="${class_pat}"/></th>
    <th><input type="text" name="timing_pat" size="6" value="${timing_pat}"/></th>
    </tr> 
  <submissions>
    <tr>
      <td><a href="/submited/${submit-id}"><submit-id/></a></td>
      <td><submit-user-id/></td>
      <td><submit-path/></td>
      <td><code-lang/></td>
      <td><classify/></td>
      <td><timing/></td>
      <td><time/></td>
    </tr>
  </submissions>
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

</apply>
