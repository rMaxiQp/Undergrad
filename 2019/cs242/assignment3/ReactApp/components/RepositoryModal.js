import React from 'react';
import {
    View,
    ScrollView,
    Dimensions,
    Modal
} from 'react-native';
import { ContributionGraph } from 'react-native-chart-kit';
import LoadingScreen from '../screens/LoadingScreen';
import request from '../utils/Request';
import aggregate from '../utils/Aggregation';

const CONFIG = {
    backgroundColor: '#E26A00',
    backgroundGradientFrom: '#FFFFFF',
    backgroundGradientTo: '#FFFFFF',
    color: (opacity = 1) => `rgba(0, 63, 19, ${opacity})`,
}

export default class RepositoryModal extends React.Component {
    constructor(props) {
        super(props);
        this.state = {
            url : props.url,
            loading : true,
            commitMap: [],
            barData: [],
        }
        this.close = props.close
    }

    componentDidMount() {
        this._fetchData();
    }

    _fetchData = () => {
        request.get(this.state.url + '/commits').then(values => {
            var commiters = values.map(value => value.commit.committer);
            
            // filter out data
            var commitDates = commiters.map(commiter => commiter.date.substring(0,10));
            
            // aggregate same data and count each element's occurance
            var countedDates = aggregate.count(commitDates);

            // parsing
            var resultDates = [];

            for(date in countedDates) {
                resultDates.push({'date': date, 'count': countedDates[date]});
            }

            this.setState({
                commitMap: resultDates, 
                loading: false,
            });

        }).catch(err => console.log(err))
    }

    render() {

        var content = <LoadingScreen />;

        if (this.state.loading === false) {
            content = (
                <ScrollView style={{backgroundColor: '#808080'}}>
                    <ContributionGraph 
                        values={this.state.commitMap}
                        endDate={new Date()}
                        numDays={180}
                        width={Dimensions.width}
                        height={400}
                        chartConfig={CONFIG}
                        />
                </ScrollView>
            );
        }

            return (
                <View onPress={this.close}>

                <Modal
                    transparent={false}
                    onRequestClose={this.close}
                    presentationStyle='pageSheet'
                    >
                    { content }
                </Modal>
                    </View>
            );
    }
}