$(document).ready(function(){
  $("#copyToClipboard").click(function(){
      var $temp = $("<textArea>");
      $("body").append($temp);
      $temp.val($('#outputMessage').text()).select();
      document.execCommand("copy");
      $temp.remove();
  });
});