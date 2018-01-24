/*
 Glue JavaScript code to startup ACE editor and submit text
 pbv, 2015
 */

/* start up the editor */
function startAceEditor(id, path) {
 var div = document.getElementById(id);
 var textarea = document.getElementsByName(id)[0]; 
 div.style.display = 'block';
 textarea.style.display = 'none';
 editor = ace.edit(div);
 editor.setFontSize(16);
 setAceEditorModeExt(id, path);
 // editor.getSession().setMode(mode);
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

/* set the editing mode by file path extension */
function setAceEditorModeExt(id, path) {
    var editor = ace.edit(id);
    var modelist = ace.require("ace/ext/modelist");
    var mode = modelist.getModeForPath(path).mode;
    editor.getSession().setMode(mode);
}

/* read a local file */
function readFile(browser, id) {
    if (window.File && window.FileReader && window.FileList && window.Blob) {
        //Get the file object
	var filesSelected = document.getElementById(browser).files;
	if(filesSelected.length > 0) {
            var fileTobeRead = filesSelected[0];
            var fileReader = new FileReader(); 
            fileReader.onload = function (e) { 
		var editor = ace.edit(id);
		editor.setValue(fileReader.result);
		editor.moveCursorTo(0,0);
		setAceEditorModeExt(id, fileTobeRead.name);
            } 
            fileReader.readAsText(fileTobeRead);
	}
    }   
}
