var editor = startAceEditor('editor');

document.getElementById('codeform').
    addEventListener('submit',function() { submitListener(editor,'code'); });
      
document.getElementById('fileselect').
    addEventListener('change', function() { fileListener(editor, 'fileselect', 'langselect'); });
      
document.getElementById('langselect').
    addEventListener('change', function() { editor.session.setMode(languageModes[this.selectedIndex]); } );
