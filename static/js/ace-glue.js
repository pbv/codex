/*
 Glue JavaScript code to startup ACE editor and submit text
 pbv, 2015 
 */

/* start up the editor */
function startAceEditor(id) {
 var textarea = document.getElementById(id);
 var div = document.getElementById(id+'.div');
 textarea.style.display = 'none';
 div.style.display = 'block';
 editor = ace.edit(div);
 editor.setFontSize(16);
}

/* set the editing mode directly */
function setAceEditorMode(id, mode) {
 var div = document.getElementById(id+'.div');
 ace.edit(div).getSession().setMode(mode);
}

/* set the editing mode from file extension */
function setAceEditorModeExt(id, other) {
    var editor = ace.edit(id + '.div');
    var path = document.getElementById(other).value;
    var modelist = ace.require("ace/ext/modelist");
    var mode = modelist.getModeForPath(path).mode;
    editor.getSession().setMode(mode);
}

/* copy text into textarea before submission */
function submitAceEditorText(id) {
  var textarea = document.getElementById(id);
  var editor = ace.edit(id+'.div');
  textarea.value = editor.getSession().getValue();
}
