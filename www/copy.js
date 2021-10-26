$(document).ready(function(){
  $("#copyToClipboard").click(function(){
      //console.log('copy button was clicked');
      var $temp = $("<input>");
      $("body").append($temp);
      $temp.val($('#outputMessage').text()).select();
      document.execCommand("copy");
      $temp.remove();
  });
});