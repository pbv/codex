<form id="codeform" method="post" action="${postAction}">
  <p><textarea name="code" rows=24 cols=80><apply-content/></textarea> 
  <p><div class="hidden" id="editor"><apply-content/></div>
  <p><input type="submit" value="${buttonText}" onClick="submitForm()"/>
</form>
<script type="text/javascript">
var form = document.forms['codeform'];
form.elements['code'].style.display = 'none';
document.getElementById('editor').style.display = 'block';

function submitForm() {
  var form = document.forms['codeform'];
  var editor = ace.edit("editor");
  form.elements['code'].value = editor.getValue();
  form.submit();
}
</script>
