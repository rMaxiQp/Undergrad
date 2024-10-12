import React from 'react';
import { Text, ScrollView, } from 'react-native';
import { ListItem } from 'react-native-elements';
import LoadingScreen from './LoadingScreen';
import request from '../utils/Request';

export default class UserScreen extends React.Component {

    constructor(props) {
        super(props);
        
        const { navigation } = this.props;

        this.state = {
            url: navigation.getParam('url', ''),
            name : '',
            loading : true,
            avatar : '',
            bio : '',
            login: '',
            company: '',
            follower: '',
            following: '',
            location: '',
        }
    }

    componentDidMount() {
        request.get(this.state.url).then(response => {

            this.setState({ 
                avatar : response['avatar_url'],
                bio : response['bio'],
                login : response['login'],
                name : response['name'],
                follower : response['followers'].toString(),
                following : response['following'].toString(), 
                company: response['company'],
                loading : false
            });
        })
    }
    
    render() {
        if (this.state.loading) {
            return (<LoadingScreen />);
        }

        return (
                <ScrollView style={{flex:1}} stickyHeaderIndices={[0]}>
                    <ListItem 
                        leftAvatar={{source: {uri: this.state.avatar}, size: 40}} 
                        title={this.state.login} 
                        titleStyle={{fontSize: 35}}
                        subtitle={this.state.name} />
                    <Text style={{alignSelf:'center'}}>{this.state.bio}</Text>
                    <ListItem 
                        leftIcon = {{
                            size: 30,
                            name: 'briefcase',
                            type: 'font-awesome'
                        }}
                        title={this.state.company}/>
                    <ListItem 
                        leftIcon = {{
                            size: 30,
                            name: 'map-pin',
                            type: 'font-awesome'
                        }}
                        title={this.state.location}/>
                    <ListItem 
                        leftIcon = {{
                            size: 30,
                            name: 'android',
                            type: 'font-awesome'
                        }}
                        title={this.state.follower}/>
                    <ListItem
                        leftIcon = {{
                            size: 30,
                            name: 'heart',
                            type: 'font-awesome'
                        }}
                        title={this.state.following}/>
                </ScrollView>
        );
    }
}