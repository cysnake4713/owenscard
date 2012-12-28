var Poker = {}
Poker.CONST = {
	BOTTOM : 'bottom',
	LEFT : 'left',
	RIGHT : 'right',
	TOPLEFT : 'topLeft',
	TOPRIGHT : 'topRight',
	POKER_WIDTH : 70,
	POKER_HEIGHT : 95,
	POKER_SPACING : 10,
	POKER_CLICK_UP_SPACING : 20,
	POKER_SHOW_SPACING : 20,

	POKER_COLOR : {
		DIAMOND : "DIAMOND",
		CLUB : "CLUB",
		HEARTS : "HEARTS",
		SPADE : "SPADE"
	}
}
Poker.Global = {
	WINDOW_HEIGHT : $(window).height() - 65,
	WINDOW_WIDTH : $(window).width()
}

Poker.Position = {

	Bottom : {
		centerX : Poker.Global.WINDOW_WIDTH / 2,
		centerY : Poker.Global.WINDOW_HEIGHT - 100
	},
	Left : {
		centerX : 90,
		centerY : Poker.Global.WINDOW_HEIGHT / 2
	},
	Right : {
		centerX : Poker.Global.WINDOW_WIDTH - 80,
		centerY : Poker.Global.WINDOW_HEIGHT / 2
	},
	TopLeft : {
		centerX : Poker.Global.WINDOW_WIDTH / 4 + 50,
		centerY : 90
	},
	TopRight : {
		centerX : (Poker.Global.WINDOW_WIDTH / 4) * 3 - 70,
		centerY : 90
	}
}

Poker.Component = {

	init : function() {
		Poker.Component.pokerImage = new Image();
		Poker.Component.stage = new Kinetic.Stage({
			container : 'container',
			width : Poker.Global.WINDOW_WIDTH,
			height : Poker.Global.WINDOW_HEIGHT
		});
		Poker.Component.pokers.init();
		for ( var i in Poker.Component.Layers) {
			Poker.Component.Layers[i].init();
		}
		Poker.Component.pokerImage.src = '/assets/images/poker.png';
	},
	pokers : {
		/*
		 * 方片: x = 0 草花: x = 1 红桃: x = 2 黑桃: x = 3 王: x = 4 (y = 0~1) 背面: x = 4
		 * (y = 2~5)
		 */

		getPokerByIndex : function(x, y) {
			var poker = new Kinetic.Image({
				image : Poker.Component.pokerImage,
				width : 70,
				height : 95,
				crop : {
					x : 70 * x,
					y : 95 * y,
					width : 70,
					height : 95
				},
				draggable : true
			});
			poker.setPosition(-100, -100);
			poker.setOffset(70 / 2, 95 / 2);
			return poker;
		},
		init : function() {
			pokerColor = Poker.CONST.POKER_COLOR;
			var t = 0
			for ( var i in Poker.CONST.POKER_COLOR) {
				this[pokerColor[i]] = {}
				for ( var j = 0; j < 13; j++) {
					this[pokerColor[i]][j] = this.getPokerByIndex(j, t);
				}
				t += 1
			}
			this["back"] = {}
			this["back"][0] = this.getPokerByIndex(3, 4);
		}
	}
}

Poker.Component.Layers = {}
function SuperLayer() {
}

SuperLayer.prototype = {
	init : function() {
		this.layer = new Kinetic.Layer();
		// add the layer to the stage
		Poker.Component.stage.add(this.layer);
	},
	clear : function() {
		this.layer.clear();
		this.layer.removeChildren();
	},
	drawByMessage : function(message, callback) {
		var pokers = Poker.tools.createPokersByMessage(message);
		this.drawByPokers(pokers, callback);
		return pokers;
	},
	drawByPokers : function(pokers, callback) {
	},
	complexText : new Kinetic.Text({
		x : -100,
		y : -100,
		padding : 5,
		text : "",
		fontSize : 20,
		textFill : 'white',
		width : 150,
		height : 100,
		align : 'center',
		// fontFamily : 'Calibri',
		// fontStyle : 'italic',
		offset : [ 75, 50 ],
		cornerRadius : 10,
	})
}

function FourLayer() {
}

FourLayer.prototype = new SuperLayer();
FourLayer.prototype.drawReceiveCards = function() {
	var msg = [ {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	} ];
	this.drawByMessage(msg);
};

FourLayer.prototype.drawShowCards = function(msg, switchTotal) {
	var msg = [ {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	}, {
		'color' : 'back',
		'number' : '0'
	} ];
	var pokers = this.drawByMessage(backs, function(e, i) {
		if (i >= 5 - switchTotal) {
			e.transitionTo({
				scale : {
					x : 0,
					y : 1
				},
				duration : 0.4
			});
		}
	});
	var father = this;
	setTimeout(function() {
		father.clear();
		father.drawByMessage(msg, function(e, i) {
			if (i >= 5 - switchTotal) {
				e.setScale({
					x : 0,
					y : 1
				});
				e.transitionTo({
					scale : {
						x : 1,
						y : 1
					},
					duration : 0.4
				});
			}
		});
	}, 500);
}

Poker.Component.Layers.dataLayer = (function() {
	function ThisLayer() {

	}

	ThisLayer.prototype = new SuperLayer();
	ThisLayer.prototype.init = function() {
		this.layer = new Kinetic.Layer();
		var la = this.layer
		Poker.Component.pokerImage.onload = function() {
			pokerArray = Poker.Component.pokers;
			for ( var i in pokerArray) {
				Poker.tools.addPokerArrayToLayer(pokerArray[i], la);
			}
			// add the layer to the stage
			Poker.Component.stage.add(la);
		}
	}
	return new ThisLayer();

})();

Poker.Component.Layers.bottomLayer = (function() {
	function ThisLayer() {

	}

	ThisLayer.prototype = new SuperLayer();
	ThisLayer.prototype.drawByPokers = function(pokers, callback) {
		Poker.tools.addPokerArrayToLayer(pokers, this.layer);
		var pokerTotal = pokers.length;
		if (pokerTotal > 0) {
			var totalLength = (pokerTotal - 1) * Poker.CONST.POKER_WIDTH
					+ Poker.CONST.POKER_SPACING * (pokerTotal - 1);
			var currentPosition = Poker.Position.Bottom.centerX - totalLength
					/ 2;
			for ( var i in pokers) {
				var poker = pokers[i];
				poker.setPosition(currentPosition,
						Poker.Position.Bottom.centerY);
				if (callback != null) {
					callback(poker);
				}
				currentPosition += Poker.CONST.POKER_WIDTH
						+ Poker.CONST.POKER_SPACING;
			}
		}
	}

	ThisLayer.prototype.drawReceiveCards = function(message) {
		return this.drawByMessage(message, function(e) {
			e.on("mouseover", function() {
				this.setScale(1.1);
				Poker.Component.stage.draw();
			});
			e.on("mouseout", function() {
				this.setScale(1);
				Poker.Component.stage.draw();
			});
			e.on("click",
					function() {
						if (this.clicked != null) {
							this.setY(this.getY()
									+ Poker.CONST.POKER_CLICK_UP_SPACING);
							Poker.Component.stage.draw();
							this.clicked = null;
						} else {
							this.clicked = "true";
							this.setY(this.getY()
									- Poker.CONST.POKER_CLICK_UP_SPACING);
							Poker.Component.stage.draw();
						}
					});
		});
	}
	// drawSwitchCards : this.drawReceiveCards
	ThisLayer.prototype.drawShowCards = function(showCards, stayCards) {
		this.drawByMessage(stayCards);
		this.drawByMessage(showCards, function(e) {
			e.setY(e.getY() - Poker.CONST.POKER_HEIGHT
					- Poker.CONST.POKER_SHOW_SPACING);
		});
	}

	return new ThisLayer();
})();

Poker.Component.Layers.leftLayer = (function() {
	function ThisLayer() {
	}

	ThisLayer.prototype = new FourLayer();
	ThisLayer.prototype.drawByPokers = function(pokers, callback) {
		Poker.tools.addPokerArrayToLayer(pokers, this.layer);
		var pokerTotal = pokers.length;
		if (pokerTotal > 0) {
			var totalLength = (pokerTotal - 1) * Poker.CONST.POKER_WIDTH
					+ Poker.CONST.POKER_SPACING * (pokerTotal - 1);
			var currentPosition = Poker.Position.Left.centerY - totalLength / 2;
			for ( var i in pokers) {
				var poker = pokers[i];
				poker.rotateDeg(90);
				poker.setPosition(Poker.Position.Left.centerX, currentPosition);
				if (callback != null) {
					callback(poker, i);
				}
				currentPosition += Poker.CONST.POKER_WIDTH
						+ Poker.CONST.POKER_SPACING;
			}
		}
	};
	return new ThisLayer();
})();

Poker.Component.Layers.rightLayer = (function() {
	function ThisLayer() {
	}

	ThisLayer.prototype = new FourLayer();
	ThisLayer.prototype.drawByPokers = function(pokers, callback) {
		Poker.tools.addPokerArrayToLayer(pokers, this.layer);
		var pokerTotal = pokers.length;
		if (pokerTotal > 0) {
			var totalLength = (pokerTotal - 1) * Poker.CONST.POKER_WIDTH
					+ Poker.CONST.POKER_SPACING * (pokerTotal - 1);
			var currentPosition = Poker.Position.Right.centerY + totalLength
					/ 2;
			for ( var i in pokers) {
				var poker = pokers[i];
				poker.rotateDeg(90);
				poker
						.setPosition(Poker.Position.Right.centerX,
								currentPosition);
				if (callback != null) {
					callback(poker, i);
				}
				currentPosition -= Poker.CONST.POKER_WIDTH
						+ Poker.CONST.POKER_SPACING;
			}
		}
	};
	return new ThisLayer();
})();

Poker.Component.Layers.topLeftLayer = (function() {
	function ThisLayer() {
	}

	ThisLayer.prototype = new FourLayer();
	ThisLayer.prototype.drawByPokers = function(pokers, callback) {
		Poker.tools.addPokerArrayToLayer(pokers, this.layer);
		var pokerTotal = pokers.length;
		if (pokerTotal > 0) {
			var totalLength = (pokerTotal - 1) * Poker.CONST.POKER_WIDTH
					+ Poker.CONST.POKER_SPACING * (pokerTotal - 1);
			var currentPosition = Poker.Position.TopLeft.centerX + totalLength
					/ 2;
			for ( var i in pokers) {
				var poker = pokers[i];
				poker.rotateDeg(180);
				poker.setPosition(currentPosition,
						Poker.Position.TopLeft.centerY);
				if (callback != null) {
					callback(poker, i);
				}
				currentPosition -= Poker.CONST.POKER_WIDTH
						+ Poker.CONST.POKER_SPACING;
			}
		}
	};
	return new ThisLayer();
})();

Poker.Component.Layers.topRightLayer = (function() {
	function ThisLayer() {
	}

	ThisLayer.prototype = new FourLayer();
	ThisLayer.prototype.drawByPokers = function(pokers, callback) {
		Poker.tools.addPokerArrayToLayer(pokers, this.layer);
		var pokerTotal = pokers.length;
		if (pokerTotal > 0) {
			var totalLength = (pokerTotal - 1) * Poker.CONST.POKER_WIDTH
					+ Poker.CONST.POKER_SPACING * (pokerTotal - 1);
			var currentPosition = Poker.Position.TopRight.centerX + totalLength
					/ 2;
			for ( var i in pokers) {
				var poker = pokers[i];
				poker.rotateDeg(180);
				poker.setPosition(currentPosition,
						Poker.Position.TopRight.centerY);
				if (callback != null) {
					callback(poker, i);
				}
				currentPosition -= Poker.CONST.POKER_WIDTH
						+ Poker.CONST.POKER_SPACING;
			}
		}
	};
	return new ThisLayer();
})();

Poker.Component.Layers.textLayer = (function() {
	function ThisLayer() {
	}

	ThisLayer.prototype = new SuperLayer();
	ThisLayer.prototype.drawNameBottom = function(name) {
		if (name != null) {
			complexText = this.complexText.clone()
			complexText.setPosition(Poker.Position.Bottom.centerX,
					Poker.Position.Bottom.centerY + Poker.CONST.POKER_HEIGHT
							+ 10)
			complexText.setText(name)
			this.layer.add(complexText)
		}
	}
	ThisLayer.prototype.drawNameLeft = function(name) {
		if (name != null) {
			complexText = this.complexText.clone()
			complexText.setPosition(Poker.Position.Left.centerX
					- Poker.CONST.POKER_HEIGHT, Poker.Position.Left.centerY)
			complexText.setText(name)
			complexText.rotateDeg(90)
			this.layer.add(complexText)
		}
	}
	ThisLayer.prototype.drawNameRight = function(name) {
		if (name != null) {
			complexText = this.complexText.clone()
			complexText.setPosition(Poker.Position.Right.centerX
					+ Poker.CONST.POKER_HEIGHT, Poker.Position.Right.centerY)
			complexText.setText(name)
			complexText.rotateDeg(270)
			this.layer.add(complexText)
		}
	}
	ThisLayer.prototype.drawNameTopLeft = function(name) {
		if (name != null) {
			complexText = this.complexText.clone()
			complexText.setPosition(Poker.Position.TopLeft.centerX,
					Poker.Position.TopLeft.centerY - 40)
			complexText.setText(name)
			this.layer.add(complexText)
		}
	}
	ThisLayer.prototype.drawNameTopRight = function(name) {
		if (name != null) {
			complexText = this.complexText.clone()
			complexText.setPosition(Poker.Position.TopRight.centerX,
					Poker.Position.TopRight.centerY - 40)
			complexText.setText(name)
			this.layer.add(complexText)
		}
	}

	return new ThisLayer();
})();

Poker.tools = {

	/*
	 * 将poker Image加入到指定Layer中
	 */
	addPokerArrayToLayer : function(pokers, layer) {
		for ( var i in pokers) {
			layer.add(pokers[i]);
		}
	},

	/*
	 * 由数组message创建poker对象
	 */
	createPokersByMessage : function(message) {
		var result = new Array();
		for ( var i in message) {
			var poker = Poker.Component.pokers[message[i]["color"]][message[i]["number"]]
					.clone();
			result.push(poker);
		}
		return result;
	}
}
