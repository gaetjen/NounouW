(function(B){B.fn.wflipbook=function(D,C){B.fn.wflipbook.imageItems=D;B.fn.wflipbook.opts=B.extend({},B.fn.wflipbook.settings,C);B.fn.wflipbook.element=this;B.fn.wflipbook.initialize()};B.fn.wflipbook.settings={initialStart:0,transSpeed:5000,fadeSpeed:"fast",maxLoop:-1};B.fn.wflipbook.initialize=function(){B.fn.wflipbook.initT=setTimeout(B.fn.wflipbook.startFlip,B.fn.wflipbook.opts.initialStart)};B.fn.wflipbook.startFlip=function(){B.fn.wflipbook.offSet=0;B.fn.wflipbook.loopCount=0;B.fn.wflipbook.flip()};B.fn.wflipbook.flip=function(){var C=B.fn.wflipbook.imageItems[B.fn.wflipbook.offSet];B(B.fn.wflipbook.element).fadeOut(B.fn.wflipbook.fadeSpeed,function(){if(A(C)){if(C.length==2){B(B.fn.wflipbook.element).html("<img src=\""+C[0]+"\" alt=\""+C[1]+"\" />").fadeIn(B.fn.wflipbook.opts.fadeSpeed)}else{B(B.fn.wflipbook.element).html("<a href=\""+C[2]+"\"><img src=\""+C[0]+"\" alt=\""+C[1]+"\" /></a>").fadeIn(B.fn.wflipbook.opts.fadeSpeed)}}else{B(B.fn.wflipbook.element).html("<img src=\""+C+"\" alt=\"\" />").fadeIn(B.fn.wflipbook.opts.fadeSpeed)}});if((B.fn.wflipbook.offSet+1)>=B.fn.wflipbook.imageItems.length){B.fn.wflipbook.offSet=0;B.fn.wflipbook.loopCount++}else{B.fn.wflipbook.offSet++}if(B.fn.wflipbook.loopCount!=B.fn.wflipbook.opts.maxLoop){B.fn.wflipbook.T=setTimeout(B.fn.wflipbook.flip,B.fn.wflipbook.opts.transSpeed)}};function A(C){if(C instanceof Array){return true}else{return false}}})(jQuery)