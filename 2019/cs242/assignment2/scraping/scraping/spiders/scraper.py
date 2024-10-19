from bs4 import BeautifulSoup
import scrapy
import re

# # python3 case
# from scraping.scraping import node

import sys

sys.path.append('..')
from node import *


# scrapy runspider scraper.py --set LOG_FILE="scraper.log"
class ScraperSpider(scrapy.Spider):
    name = "Scraper"

    def __init__(self, *args, **kwargs):
        self.url = kwargs.get('url', [])
        self.actor_counter = 0
        self.film_counter = 0
        super(ScraperSpider, self).__init__(*args, **kwargs)

    ''' 
    start_requests is called when we first start scraping
    1. it loads the start_urls
    2. start request url    
    '''

    def start_requests(self):
        start_urls = [
            # 'https://en.wikipedia.org/wiki/Morgan_Freeman'
            # 'https://en.wikipedia.org/wiki/Meryl_Streep'
            'https://en.wikipedia.org/wiki/Chris_Evans_(actor)'
        ]

        for url in start_urls:
            self.logger.info('Starting request with %s', url)
            yield scrapy.Request(url, self.parse_actor)

    '''
    parse should take response of an external link, and
    1. parse html and find film table or return
    2. request films in film table
    '''

    def parse(self, response):

        if (self.actor_counter > 250) and (self.film_counter > 125):
            return

        self.logger.debug('Parse external Filmography link %s', response.url)

        soup = BeautifulSoup(response.text, 'html.parser')

        actor_name = ''

        try:
            actor_name = soup.find('div', {'class': 'mw-parser-output'}).find('a').text
        except Exception as e:
            self.logger.error("Parsing actor name in {} failed: {}".format(response.url, e.args))

        try:
            table = soup.find('table', {'class': 'plainrowheaders'})

            i_tags = table.find('tbody').find_all('i')

            raw_films = list(filter(lambda raw_film: raw_film is not None,
                                    list(map(lambda i_tag: i_tag.find('a', href=True), i_tags))))

            films = list(map(lambda film: film['href'], raw_films))

            self.logger.info("Actor %s in %s has films %s ", actor_name, response.url, films)

            file = '../actor/' + actor_name.replace(' ', '_') + '.txt'
            obj = node()
            obj.load_json(file)
            obj.add_dependency(films)
            obj.to_json(outfile)

            for film in films:
                link = response.urljoin(film)

                yield scrapy.Request(link, callback=self.parse_film)

        except Exception as e:
            self.logger.error("An Exception Occured when parsing {}: {}".format(response.url, e.args))

    '''
    parse_actor should take response of the given web page, and
    1. log into a logging file
    2. store film and push films into queue
    '''

    def parse_actor(self, response):

        self.actor_counter += 1

        def get_film_table(film_table):
            result = []
            i_tags = film_table.find('tbody').find_all('i')
            for i_tag in i_tags:
                current_link = i_tag.find('a', href=True)
                if current_link is not None:
                    result.append(current_link['href'])
            return result

        def get_actor_name(box):
            return box.find_all('tr')[0].text

        def get_actor_age(box):
            age = list(filter(lambda x: 'age' in x, re.findall('\((.*?)\)', box.text)))[0]
            return re.search('\d+', age).group(0)

        if (self.actor_counter > 250) and (self.film_counter > 125):
            return

        self.logger.debug("Visiting %s", response.url)

        soup = BeautifulSoup(response.text, 'html.parser')

        try:
            actor_name = get_actor_name(soup.find('table', {'class': 'infobox'}))

            film_tag_location = soup.find('span', {'id': re.compile('^Film')}).parent

            while True:

                if film_tag_location.name == 'table':
                    break

                if film_tag_location.name == 'div':
                    tag_class = film_tag_location['class']
                    if 'hatnote' in tag_class:
                        break

                film_tag_location = film_tag_location.next_sibling

            films = []
            if film_tag_location.name == 'div':

                external_link = film_tag_location.find('a')['href']
                external_link = response.urljoin(external_link)

                file = '../actor/' + actor_name.replace(' ', '_') + '.txt'
                with open(file, 'w+') as outfile:
                    obj = node(link=response.url.replace('https://en.wikipedia.org', ''), name=actor_name,
                               dependency=films, year=get_actor_age(soup.find('table', {'class': 'infobox'})))
                    obj.to_json(outfile)


                yield scrapy.Request(external_link, callback=self.parse)

            elif film_tag_location.name == 'table':
                films = get_film_table(film_tag_location)

            if films is None:
                self.logger.error('No film found...')
                return
            else:

                file = '../actor/' + actor_name.replace(' ', '_') + '.txt'

                self.logger.info("Actor %s in %s has films %s ", actor_name, response.url, films)

                with open(file, 'w+') as outfile:
                    obj = node(link=response.url.replace('https://en.wikipedia.org', ''), name=actor_name,
                               dependency=films, year=get_actor_age(soup.find('table', {'class': 'infobox'})))
                    obj.to_json(outfile)

                for film in films:
                    film_link = response.urljoin(film)

                    yield scrapy.Request(film_link, callback=self.parse_film)
        except Exception as e:
            self.logger.error("An Exception Occured when parsing {}: {}".format(response.url, e.args))

    '''
    parse_film should take response of the given web page, and
    1. log into a logging file
    2. find box office
    3. store actor and push actors into queue
    '''

    def parse_film(self, response):

        def get_film_name(box):
            box_titles = box.find_all('th')

            name = list(filter(lambda title: ('class' in title.attrs) and ('summary' in title['class']), box_titles))

            if name is not None:
                return name[0].text

            return ''

        def get_box_office(box):
            power = {'billion': 10 ** 9, 'million': 10 ** 6}

            if box is None:
                return 0

            box_titles = box.find_all('th')

            grossing_value = list(filter(lambda title: title.text == "Box office", box_titles))

            if len(grossing_value) > 0:

                price_tag = grossing_value[0].next_sibling

                if 'million' in price_tag.text:
                    amount = re.search(r"([0-9.]+)\s?(million|billion)", price_tag.text)
                else:
                    price_text = price_tag.text.replace(',', '')
                    amount = re.search(r'([0-9.]+)', price_text)
                    return amount.group(0)

                if amount is not None:
                    quantity = amount.group(1)
                    magnitude = amount.group(2)
                    return float(quantity) * power[magnitude]
            return 0

        def get_year(box):
            if box is None:
                return 1990

            box_titles = box.find_all('th')

            release_date = list(filter(lambda title: title.text == "Release date", box_titles))

            if len(release_date) > 0:

                price_tag = release_date[0].next_sibling

                amount = re.search(r"\d{4}", price_tag.text)

                if amount is not None:
                    return amount.group(0)

            return 1990

        def get_link(title_list, title_text):

            symbols = list(filter(lambda title: title.text == title_text, title_list))

            if len(symbols) > 0:
                raw_links = symbols[0].next_sibling.find_all('a', href=True)

                links = list(map(lambda anchor: anchor['href'], raw_links))

                return links

            return []

        self.film_counter += 1

        if (self.actor_counter > 250) and (self.film_counter > 125):
            return

        self.logger.info('Parsing film: %s', response.url)

        soup = BeautifulSoup(response.text, 'html.parser')

        infobox = soup.find("table", {"class": "infobox"})

        film_name = get_film_name(infobox)

        box_office = get_box_office(infobox)

        if box_office == 0:
            self.logger.warning("No Box Office Found...")

        year = get_year(infobox)

        try:
            titles = infobox.find_all('th')

            narrators = get_link(titles, "Narrated by")

            if not narrators:
                self.logger.warning("No Narrator Found...")

            actors = get_link(titles, "Starring")

            if not actors:
                self.logger.warning("No Actor Found...")

            self.logger.info("Film: %s has actors %s with box office %s", response.url, (narrators + actors),
                             box_office)

            file = '../film/' + film_name.replace(' ', '_') + '.txt'

            with open(file, 'w+') as outfile:
                obj = node(link=response.url.replace('https://en.wikipedia.org', ''), value=box_office, year=year,
                           name=film_name, dependency=(narrators + actors))
                obj.to_json(outfile)

            for actor in (narrators + actors):
                actor_link = response.urljoin(actor)

                yield scrapy.Request(actor_link, callback=self.parse_actor)

        except Exception as e:
            self.logger.error("An Exception Occured when parsing {}: {}".format(response.url, e.args))
