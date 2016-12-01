/*
 Glue JavaScript code to startup ACE editor and submit text
 pbv, 2015 
 */

/* start up the editor */
function startAceEditor(id, mode) {
 var div = document.getElementById(id);
 var textarea = document.getElementsByName(id)[0]; 
 div.style.display = 'block';
 textarea.style.display = 'none';
 editor = ace.edit(div);
 editor.setFontSize(16);
 editor.getSession().setMode(mode);
}

/* copy text into textarea before submission */
function submitAceEditorText(id) {
  var textarea = document.getElementsByName(id)[0];
  var editor = ace.edit(id);
  textarea.value = editor.getSession().getValue();
}


/* set the editing mode directly; NB: this is not used anymore? */
function setAceEditorMode(id, mode) {
 var div = document.getElementById(id);
 ace.edit(div).getSession().setMode(mode);
}

/* set the editing mode from file extension */
function setAceEditorModeExt(id, other) {
    var editor = ace.edit(id);
    var path = document.getElementById(other).value;
    var modelist = ace.require("ace/ext/modelist");
    var mode = modelist.getModeForPath(path).mode;
    editor.getSession().setMode(mode);
}

