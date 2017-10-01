<apply template="_base">
<h1>Submissões</h1>
<form id="listform" action="/submissions" method="POST">
  <table class="submissions">
    <tr>
      <td colspan="6">
	<span class="info">
	  <submissions-count/> submissões (página <page/> de <page-count/>)
	</span> &nbsp;
	<select name="sorting" onChange="this.form.submit()">
	  <if-ascending>
	    <option value="Asc" selected>ascendente</option>
	    <option value="Desc">descendente</option>
	    <else/>
	    <option value="Asc">ascendente</option>
	    <option value="Desc" selected>descendente</option>
	  </if-ascending>
	</select> &nbsp;
	<a class="button" href="${submissions-prev-url}">&lt;</a> &nbsp;
	<a class="button" href="${submissions-next-url}">&gt;</a> &nbsp;
	<input type="submit" value="Filtrar"/>
      </td>
      <td colspan="1">
	<input type="button" onClick="reevaluate()" value="Re-avaliar"/>
	<input type="button" onClick="cancel()" value="Cancelar"/>	
      </td>
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
      <th><input type="text" name="id" size="4" value="${id}"/></th>
      <th><input type="text" name="user_id" size="8" value="${user_id}"/></th>
      <th><input type="text" name="path" size="20" value="${path}"/></th>
      <th><input type="text" name="language" size="8" value="${language}"/></th>
      <th><input type="text" name="class" size="20" value="${class}"/></th>
      <th><input type="text" name="timing" size="6" value="${timing}"/></th>
      <th/>
    </tr>
    <if-submissions>
      <submissions>
	<tr>
	  <td class="submitid"><a href="${submit-url}"><submit-id/></a></td>
	  <td class="userid"><submit-user-id/></td>
	  <td class="path"><a href="/pub/${submit-path}"><submit-path/></a></td>
	  <td class="lang"><code-lang/></td>
	  <td class="classify ${submit-classify}"><submit-classify/></td>
	  <td class="timing"><submit-timing/></td>
	  <td class="received"><submit-time/></td>
	</tr>
      </submissions>
      <else/>
      <tr><td colspan="7" align="center">(No submissions)</td></tr>
    </if-submissions>
  </table>
  <input id="page" type="hidden" name="page" value="${page}"/>
  <input id="_method" type="hidden" name="_method" value="GET"/>
</form>
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
