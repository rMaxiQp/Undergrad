import React from 'react';
import { View, ScrollView, RefreshControl, AsyncStorage, Text } from 'react-native';
import { ListItem, Icon } from 'react-native-elements';
import { Constants } from 'expo';
import LoadingScreen from './LoadingScreen';
import request from '../utils/Request';
import Swipeout from 'react-native-swipeout';

export default class StarScreen extends React.Component {

    static navigationOptions = ({ navigation }) => {
        const { params = {} } = navigation.state;

        return {
            headerTitle: (<Text style={{ color: '#fff' }}> Stars </Text>),
            headerRight: (<Icon name='undo' type='font-awesome' color={'#fff'} disabled={params.undoDisable} onPress={params._undo} />)
        };
    };

    constructor() {
        super();
        this.state = {
            stars: [],
            loading: true,
            unstarred: []
        };
    }
    
    componentDidMount() {
        this._fetchStars();
        this.props.navigation.setParams({ _undo: this._undo, undoDisable: true });
    }

    _fetchStars = () => {
        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.fetchInfo(values[0], values[1], '/starred').then(response => {

                stars = response.map(star => {
                    return {
                        name: star.full_name,
                        description: star.description,
                        url: star.url,
                        starCount: star.stargazers_count
                    };
                })

                if (this.state.unstarred.length === 0) {
                    this.props.navigation.setParams({ undoDisable: true })
                }

                this.setState({ stars: stars, loading: false });
            })
         }).catch(err => console.log(err))
    }

    _refresh = () => {
        this.setState({ loading: true });
        this._fetchStars();
    }

    _undo = () => {

        this.setState({ loading: true });

        var newUnstars = this.state.unstarred;
        var undoElement = newUnstars.pop();

        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.addInfo(values[0], values[1], '/starred/' + undoElement).then(() => {
                this.setState({ unstarred: newUnstars })
                this._fetchStars();
            })
        }).catch(err => console.log(err))
    }

    _unstar = (index) => {
        this.setState({loading : true})

        var newStars = this.state.stars;
        var newUnstars = this.state.unstarred;
        var unstarRepo = newStars.splice(index, 1)[0];
        newUnstars.push(unstarRepo.name);

        Promise.all([AsyncStorage.getItem('user'), AsyncStorage.getItem('pass')]).then(values => {
            request.deleteInfo(values[0], values[1], '/starred/' + unstarRepo.name).then(() => {

                if (newUnstars.length === 1) { this.props.navigation.setParams({ undoDisable: false }) }

                this.setState({ loading: false, stars : newStars, unstarred : newUnstars });
            }).catch(err => console.log(err))
        })
    }

    _renderStar(star, index) {

        var deleteButton = [{
            text: 'unstar',
            backgroundColor: 'red',
            underlayColor: '#fff',
            onPress: () => this._unstar(index)
        }]

        return (
            <Swipeout
                key={index}
                autoClose={true}
                right={deleteButton}
            >
                <ListItem
                    title={star.name}
                    subtitle={star.description}
                    badge={{ value: star.starCount, status: 'warning' }} 
                />
            </Swipeout>
        );
    }

    render() {
        if (this.state.loading) { return (<LoadingScreen />); }

        return (
            <View style={{ flex: 1, paddingHeight: Constants.statusBarHeight,}}>
                <ScrollView refreshControl={<RefreshControl refreshing={this.state.loading} onRefresh={this._refresh} />} >
                    {this.state.stars.map((star, index) => this._renderStar(star, index))}
                </ScrollView>
            </View>
        );
    }
}