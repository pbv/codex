<form id="codeform" method="post" action="${post_action}">
  <p><textarea name="code" rows=24 cols=80><apply-content/></textarea> 
  <p><div class="hidden" id="editor"><apply-content/></div>
  <p><input type="hidden" name="path" value="${edit_path}"/>
  <p><input type="submit" value="${submit_label}" onClick="submitForm()"/>
</form>
<script src="/ace-builds/src-min-noconflict/ace.js" type="text/javascript" charset="utf-8"></script>
<script src="/ace-builds/src-min-noconflict/ext-modelist.js" type="text/javascript" charset="utf-8"></script>
<script type="text/javascript">
var editor = ace.edit("editor");
editor.setFontSize(16);

var form = document.forms['codeform'];
form.elements['code'].style.display = 'none';
document.getElementById('editor').style.display = 'block';

var path = form.elements['path'].value; 
var modelist = ace.require("ace/ext/modelist");
var mode = modelist.getModeForPath(path).mode;
editor.getSession().setMode(mode);

function submitForm() {
  var form = document.forms['codeform'];
  var editor = ace.edit("editor");
  form.elements['code'].value = editor.getValue();
  form.submit();
}
</script>
