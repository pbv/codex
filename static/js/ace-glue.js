/*
 Glue JavaScript code to startup ACE editor and submit text
 pbv, 2018
 */

/* start up the editor */
function startAceEditor(id) {
    var editor = ace.edit(id);
    var size = window.localStorage.getItem('fontsize');
    if (size == null)
	size = "16";
    editor.setFontSize(parseInt(size));
    editor.getSession().setUseSoftTabs(true);
    return editor;
}

function changeFontSize(id, delta) {
    var size = editor.getFontSize() + delta;
    if (size < 8)
	size = 8;
    else if (size > 40)
	size = 40;
    editor.setFontSize(size);
    window.localStorage.setItem('fontsize',size);
}


/* set the editing mode by file path extension 
*/

function setAceEditorModeExt(editor, path) {
    var modelist = ace.require("ace/ext/modelist");
    var mode = modelist.getModeForPath(path).mode;
    editor.getSession().setMode(mode);
    editor.getSession().setUseSoftTabs(true);
}


/* read a local file
 */
function readFile(editor, browserId) {
    if (window.File && window.FileReader && window.FileList && window.Blob) {
        // Get the file object
	var filesSelected = document.getElementById(browserId).files;
	if(filesSelected.length > 0) {
            var fileTobeRead = filesSelected[0];
            var fileReader = new FileReader(); 
            fileReader.onload = function (e) { 
		editor.setValue(fileReader.result);
		editor.moveCursorTo(0,0);
		editor.getSession().setUseSoftTabs(true);
            } 
            fileReader.readAsText(fileTobeRead);
	    return fileTobeRead.name;
	} 
    }   
}


function submitListener(editor, codeId) {
    var textarea = document.getElementById(codeId);
    textarea.value = editor.session.getValue();
}

function fileListener(editor, fileBrowserId, langSelectorId) {
    var filename = readFile(editor, fileBrowserId);
    for(var i = 0; i<languageExtensions.length; i++)  {
	if ( filename.endsWith(languageExtensions[i]) ) {
	    var input = document.getElementById(langSelectorId);
	    editor.session.setMode(languageModes[i]);
	    input.selectedIndex = i;
	    break;
	}
    }    
}

