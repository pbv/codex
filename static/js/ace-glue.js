/*
 Glue JavaScript code to startup ACE editor and submit text
 pbv, 2018
 */

/* start up the editor */
function startAceEditor(id) {
    var editor = ace.edit(id);
    editor.setFontSize(16);
    return editor;
}

/* set the editing mode by file path extension 
*/

function setAceEditorModeExt(editor, path) {
    var modelist = ace.require("ace/ext/modelist");
    var mode = modelist.getModeForPath(path).mode;
    editor.getSession().setMode(mode);
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

