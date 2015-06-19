<form id="codeform" method="post" action="${postAction}">
  <p><textarea name="code" rows=40 cols=100><apply-content/></textarea> 
  <p><div class="hidden" id="editor"><apply-content/></div>
  <p><input type="submit" value="${buttonText}" onClick="submitForm()"/>
</form>
<script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
<script type="text/javascript">
var editor = ace.edit("editor");
editor.setFontSize(16);

var form = document.forms['codeform'];
form.elements['code'].style.display = 'none';
document.getElementById('editor').style.display = 'block';
var mode = form.editMode;
editor.getSession().setMode("ace/mode/markdown");

function submitForm() {
  var form = document.forms['codeform'];
  var editor = ace.edit("editor");
  form.elements['code'].value = editor.getValue();
  form.submit();
}
</script>
