<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="icon-warning">&#9888;</bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>
<apply template="_base">
  <apply template="_browse"/>
  <page-title/>
<h2>Submission <submit-id/></h2>
<p>Submited by <code><submit-user-id/></code> (<submit-user-name/>) in
<submit-time/>.</p>
<if-evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Reload this page if it doesn't update automatically.</p>
<else/>
<h3>Result: <em><result-status/></em></h3>
<if-valid><else/><p><icon-warning/>Invalid submission: <em><invalid-msg/></em></p></if-valid>
<result-private-report/>
<h3>Code</h3>
<pre><submit-code/></pre> 
<hr/>
<script type="text/javascript">
  function confirmDelete() {
  var r = confirm("Delete this submission (this operation is NOT reversable)?");
  if (r) {
  var form = document.getElementById("deleteform");
  form.submit();
  }
  }
</script>

  <a class="button" type="button" onclick="window.open('${report-url}')">View</a> &nbsp;
  <form id="deleteform" method="POST" 
	action="${submission-admin-url}" style="display:inline;">
    <input type="hidden" name="_method" value="DELETE"/>
    <input type="button" onClick="confirmDelete()" 
	   title="Delete submission" value="Delete"/>
  </form> &nbsp;
  <form method="POST" action="${submission-admin-url}" style="display:inline;">
  <input type="hidden" name="_method" value="PATCH"/>
  <input type="submit" title="Re-evaluate the submission" value="Re-evaluate"/>
  </form>

</if-evaluating>
</apply>

