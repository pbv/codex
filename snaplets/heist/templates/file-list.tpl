<bind tag="icon-folder"><img src="/static/icons/16x16/folder.png"></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/text-editor.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<apply template="_base">
<h1>/<file-path/></h1>

<table class="filelist">
  <tr>
    <td/> <th>Nome</th> <th>Tipo</th>  <th>Última alteração</th>
  </tr>
  <file-list>
    <tr>
      <td>
	<if-text><icon-editor/></if-text>
	<if-dir><icon-folder/></if-dir>
      </td>
      <td class="filename">
	  <a href="/files/${file-path-url}"><file-name/></a>
      </td>
      <td class="filetype"><file-type/></td>
      <td class="filemodified"><file-modified/></td>
    </tr>
  </file-list>
</table>
<hr/>
<form action="/files/${file-path}" enctype="multipart/form-data" method="POST">
<input type="file" name="datafile" required="required"/>  &nbsp;
<input type="submit" value="Upload"/>
</form>
<div class="warnings">
<dl>
  <message-list>
    <dd><icon-warning/>&nbsp;<message/></dd>
  </message-list>
</dl>
</div>
</apply>

<apply template="_browse">
</apply>
