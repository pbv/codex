<apply template="_base">
<h1>Submissões</h1>
<form id="listform" action="/submit" method="POST">
  <table class="submissions">
    <tr><th colspan="4">
	<span class="info">
	  <submissions-count/> submissões (página <page/> de <page-count/>)
	</span>
	<select name="sorting" onChange="this.form.submit()">
	  <if-ascending>
	    <option value="Asc" selected>ascendente</option>
	    <option value="Desc">descendente</option>
	    <else/>
	    <option value="Asc">ascendente</option>
	    <option value="Desc" selected>descendente</option>
	  </if-ascending>
	</select> 
      </th>
      <th colspan="3">
	<input type="submit" onClick="move(-1)" value="Anterior"/> &nbsp;
	<input type="submit" onClick="move(1)" value="Seguinte"/> &nbsp;
	<input type="submit" onClick="reevaluate()" value="Re-avaliar"/>
	<input type="submit" onClick="cancel()" value="Cancelar"/>
      </th>
    </tr>
    <tr>
      <th>id</th>
      <th>user_id</th>
      <th>path</th>
      <th>language</th>
      <th>classify</th>
      <th>timing</th>
      <th>received</th>
    </tr>
    <tr>
      <th><input type="text" name="id_pat" size="4" value="${id_pat}"/></th>
      <th><input type="text" name="uid_pat" size="8" value="${uid_pat}"/></th>
      <th><input type="text" name="path_pat" size="20" value="${path_pat}"/></th>
      <th><input type="text" name="lang_pat" size="8" value="${lang_pat}"/></th>
      <th><input type="text" name="class_pat" size="20" value="${class_pat}"/></th>
      <th><input type="text" name="timing_pat" size="6" value="${timing_pat}"/></th>
      <th/>
    </tr>
    <if-submissions>
      <submissions>
	<tr>
	  <td class="submitid"><a href="/submit/${submit-id}"><submit-id/></a></td>
	  <td class="userid"><submit-user-id/></td>
	  <td class="path"><a href="/files/${submit-path}"><submit-path/></a></td>
	  <td class="lang"><code-lang/></td>
	  <td class="classify ${submit-classify}"><submit-classify/></td>
	  <td class="timing"><submit-timing/></td>
	  <td class="received"><submit-time/></td>
	</tr>
      </submissions>
      <else/>
      <tr><td colspan=7 align="center">(No submissions)</td></tr>
    </if-submissions>
  </table>
  <input id="page" type="hidden" name="page" value="${page}"/>
  <input id="_method" type="hidden" name="_method" value="POST"/>
</form>
<script>
function move(k) {
  var form = document.getElementById("listform");
  var param = document.getElementById("page");
  var page = parseInt(param.value, 10);
  param.value = String(page + k);
  form.submit();
}
function reevaluate() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "PATCH";
  form.submit();
}
function cancel() {
  var form = document.getElementById("listform");
  var param = document.getElementById("_method");
  param.value = "DELETE";
  form.submit();
}
</script>

<hr/>

<form action="/export" method="POST" style="display:inline;">
 <input type="submit" title="Exportar como texto" value="Exportar"/> separador:
 <select name="sep">
    <option value=",">vírgula (,)</option>
    <option value=";">ponto-e-vírgula (;)</option>
    <option value="&#9">tabulação</option>
  </select>
</form>

</apply>

<apply template="_browse">
</apply>
