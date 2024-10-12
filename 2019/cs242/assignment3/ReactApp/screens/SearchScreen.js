import React from 'react';
import { View, ScrollView, Text } from 'react-native';
import { ListItem, SearchBar } from 'react-native-elements';
import { Constants } from 'expo';
import request from '../utils/Request';
import RepositoryModal from '../components/RepositoryModal';

export default class SearchScreen extends React.Component {

    // Customize header bar to cetner title and enable onPress()
    static navigationOptions = ({ navigation }) => {
        const { params = {} } = navigation.state;

        return {
            header: (
                <View
                    style={{
                        height: '8%',
                        minHeight: Constants.statusBarHeight,
                        marginTop: 20,
                        backgroundColor: '#0080FF',
                        justifyContent: 'center',
                    }}>
                    <Text
                        onPress={params._flip}
                        style={{ 
                            color: '#FFF', 
                            fontWeight:'bold', 
                            textAlign: 'center' 
                            }}>
                    {params.title} 
                    </Text>
                </View>
            )
        };
    };

    constructor() {
        super();
        this.state = {
            loading : false,
            query : '',
            searchUser : false,
            resultList : [],
            modalIndex : -1,
        }
    }

    componentDidMount() {
        this.props.navigation.setParams({ _flip: this._flip, title: 'Search on Repository' });
    }

    _flip = () => {
        const currentState = this.state.searchUser;
        const newTitle = currentState === true ? 'Search on Repository' : 'Search on User';
        this.setState({ searchUser : !currentState, query: '', resultList: []});
        this.props.navigation.setParams({ title: newTitle })
    }

    _fetchRepoSerch = ( input ) => {
        var query = input.replace(' ', '+');
        var url = 'https://api.github.com/search/repositories?q=' + query;
        
        request.get(url).then(values => {
            var repos = values.items;

            var list = repos.map(repo => {
                return {
                    name: repo.full_name,
                    description: repo.description,
                    url: repo.url,
                    starCount: repo.stargazers_count
                }
            })

            this.setState({ loading: false, resultList: list });
        }).catch(err => console.log(err))
    }

    _fetchUserSearch = ( input ) => {
        var query = input.replace(' ', '+');
        var url = 'https://api.github.com/search/users?q=' + query;

        request.get(url).then(values => {
            var users = values.items;

            var list = users.map(user => {
                return {
                    avatar: user.avatar_url,
                    name: user.login,
                    url: user.url
                };
            })

            this.setState({ loading: false, resultList: list });
        }).catch(err => console.log(err))
    }

    _updateSerach = (input) => {
        input.preventy
        this.setState({ query : input, loading : true });
        if (this.state.searchUser === true) {
            this._fetchUserSearch( input );
        } else {
            this._fetchRepoSerch( input );
        }
    }

    _mapUserResult = () => {

        var users = this.state.resultList;

        return users.map((user, index) => 
            <ListItem
                key={index}
                title={user.name}
                leftAvatar={{ source: { uri: user.avatar } }}
                onPress={() => this.props.navigation.navigate('UserProfile', { url: user.url })}
            />)
    }

    _mapRepoResult = () => {

        var repos = this.state.resultList;

        return repos.map((repo, index) =>
            <ListItem
                key={index}
                title={repo.name}
                subtitle={repo.description}
                badge={{ value: repo.starCount, status: 'warning' }}
                onPress={() => this.setState({modalIndex : index})}
            />
        );
    }

    _closeModal = () => {
        this.setState({ modalIndex : -1 })
    }

    render() {
        const {modalIndex, resultList, loading, searchUser, query} = this.state;

        var modal = <View></View>;
        if (modalIndex !== -1) {
            modal = <RepositoryModal url={resultList[modalIndex].url} close={this._closeModal} />
        }

        return (
            <View>
                { modal }
                <SearchBar 
                    placeholder="Type Here..."
                    onChangeText={this._updateSerach}
                    value={query}
                    showLoading = {loading}
                    lightTheme = {searchUser}
                    round
                    />
                    <ScrollView>
                        {this.state.searchUser ? this._mapUserResult() : this._mapRepoResult() }
                    </ScrollView>
            </View>
            );
    }

};