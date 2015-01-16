var Topics, Topic;

var Panel = ReactBootstrap.Panel;

Topic = React.createClass({
  handleClick: function(e) {
    var event = new CustomEvent('clickTopic',
                                { 'detail': this.props.topic });
    this.props.emit(event);
  },
  handleDragStart: function(e) {
    console.log(this.props.topic)
    var event = new CustomEvent('dragStartTopic',
                                { 'detail': this.props.topic});
    this.props.emit(event);
  },
  handleDragEnd: function(e) {
    var event = new CustomEvent('dragEndTopic',
                                { 'detail': this.props.topic});
    this.props.emit(event);
  },
  render: function(){
    return (
    <div
      className={"topic"}
      onClick={this.handleClick}
      draggable="true"
      onDragStart={this.handleDragStart}
      onDragEnd={this.handleDragEnd}
    >
        <div className="description"> Thema: {this.props.topic.description}</div>
        <div className="typ"> Typ: {this.props.topic.typ}</div>
    </div>
    );
  }
});

Topics = React.createClass({
  emit : function(event){
    this.getDOMNode().dispatchEvent(event);
  },
  render: function(){
    var topics = this.props.topics
      .map(function(topic){
        return (
            <Topic topic={topic} key={topic.description} emit={this.emit} />
        );
      },this);
    return (
        <Panel id="topicsContainer" header="Themen" bsStyle="primary">
        <div id="topic">
          {topics}
        </div>
        </Panel>
    );
  }
});
