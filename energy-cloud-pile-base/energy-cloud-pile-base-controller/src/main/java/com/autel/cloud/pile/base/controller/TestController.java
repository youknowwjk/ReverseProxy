package com.autel.cloud.pile.base.controller;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.autel.cloud.base.common.MessageSourceHolder;
import com.autel.cloud.base.common.util.UserUtil;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.model.OrderCommodityDetail;
import com.autel.cloud.pile.base.domain.model.SubscribeInfoDetail;
import com.autel.cloud.pile.base.domain.model.SubscribedInvoiceDTO;
import com.autel.cloud.pile.base.domain.repository.*;
import com.autel.cloud.pile.base.domain.service.OpLocationPriceService;
import com.autel.cloud.pile.base.domain.service.SubscribePlaceOrderService;
import com.autel.cloud.pile.base.domain.service.impl.UserTimezoneServiceImpl;
import com.autel.cloud.pile.base.domain.utils.AutelThreadUtils;
import com.autel.cloud.pile.base.domain.utils.TariffUtil;
import com.autel.cloud.pile.base.domain.utils.ThreadPoolTaskExeWithMdc;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.enums.AllocationStrategyEnums;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileEvseElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationPileGroupAssociateElastic;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileEvseElasticDTO;
import com.autel.cloud.pile.base.infrastructure.elastic.entity.OpLocationPileGroupAssociateElasticDTO;
import com.autel.cloud.pile.base.infrastructure.feign.adapter.PileUserServiceAdapter;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileGroupMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.OpLocationPileVipConfigMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.*;
import com.autel.cloud.pile.base.vo.OpLocationPriceDetailVO;
import com.autel.cloud.pile.base.vo.OpLocationPriceVO;
import com.autel.cloud.pile.bill.dto.TimezoneUserDTO;
import com.autel.cloud.pile.bill.enums.PileBillExceptionEnum;
import com.autel.cloud.pile.user.api.dto.EmailSendDTO;
import com.autel.cloud.tariff.dto.CostModelRuleDTO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.CollectionUtils;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.apache.skywalking.apm.toolkit.trace.RunnableWrapper;
import org.apache.skywalking.apm.toolkit.trace.TraceContext;
import org.elasticsearch.index.query.BoolQueryBuilder;
import org.elasticsearch.index.query.QueryBuilders;
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.springframework.amqp.core.MessagePostProcessor;
import org.springframework.amqp.core.MessageProperties;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.elasticsearch.core.ElasticsearchRestTemplate;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.annotation.Resource;
import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

import static com.autel.cloud.pile.base.domain.constant.SubscribeInvoiceConstant.*;
import static com.autel.cloud.pile.base.domain.service.impl.EmailSendingServiceImpl.*;

/**
 * @ClassName TestController
 * @Author A22121
 * @Description
 * @Date 2022/4/29 15:24
 * @Version 0.0.1-SNAPSHOT
 */
@RestController
@Validated
@Api(value = "测试", tags = "测试")
@RequestMapping("/test")
@Slf4j
public class TestController {

    @Resource
    private PileUserServiceAdapter pileUserServiceAdapter;

    @Resource
    private OpLocationElastic opLocationElastic;
    @Resource
    private OpLocationEvseElastic opLocationEvseElastic;
    @Resource
    private OpLocationPileEvseElastic opLocationPileEvseElastic;
    @Resource
    private OpLocationPileGroupAssociateElastic opLocationPileGroupAssociateElastic;

    @Resource
    private ElasticsearchRestTemplate elasticsearchRestTemplate;

    @Resource
    private OpLocationRepository opLocationRepository;
    @Resource
    private OpLocationEvseRepository opLocationEvseRepository;
    @Resource
    private OpLocationPileEvseRepository opLocationPileEvseRepository;
    @Resource
    private OpLocationPileGroupAssociateRepository opLocationPileGroupAssociateRepository;
    @Resource
    OpLocationPileGroupRepository opLocationPileGroupRepository;

    @GetMapping("/opLocationPileGroup")
    public ResponseEntity<Object> opLocationPileGroup(@RequestParam("id") Long id) {
        OpLocationPileGroupEntity byId = opLocationPileGroupRepository.getById(id);
        return ResponseEntity.ok(byId);
    }

    @Autowired
    private ThreadPoolTaskExeWithMdc threadPoolTaskExeWithMdc;
    @Resource
    private OpLocationPileGroupMapper opLocationPileGroupMapper;

    @Resource
    private UserTimezoneServiceImpl userTimezoneServiceImpl;


    @Resource
    private OpLocationPriceService opLocationPriceService;

    @Autowired
    private SubscribePlaceOrderService subscribePlaceOrderService;

    private MessagePostProcessor messagePostProcessor = (message) -> {
        MessageProperties messageProperties = message.getMessageProperties();
        messageProperties.setCorrelationId(org.springframework.util.StringUtils.hasText(TraceContext.traceId()) ? TraceContext.traceId() : "N/A");
        return message;
    };
    @Resource
    private RabbitTemplate rabbitTemplate;

    private static LocationPriceDTO toLocationPriceDto(OpLocationPriceVO opLocationPriceVO) {
        LocationPriceDTO dto = new LocationPriceDTO();
        BeanUtils.copyProperties(opLocationPriceVO, dto);
        List<OpLocationPriceDetailVO> priceDetails = opLocationPriceVO.getPriceDetails();
        if (CollectionUtils.isNotEmpty(priceDetails)) {
            dto.setPriceDetails(priceDetails.stream().map(vo -> {
                LocationPriceDetailDTO priceDetailDTO = new LocationPriceDetailDTO();
                BeanUtils.copyProperties(vo, priceDetailDTO);
                List<OpLocationPriceDetailVO.DayPriceVO> dayPriceList = vo.getDayPriceList();
                priceDetailDTO.setDayPriceList(dayPriceList.stream().map(dayPriceVO -> {
                    LocationPriceDetailDTO.DayPriceDTO priceDTO = new LocationPriceDetailDTO.DayPriceDTO();
                    BeanUtils.copyProperties(dayPriceVO, priceDTO);
                    return priceDTO;
                }).collect(Collectors.toList()));
                return priceDetailDTO;
            }).collect(Collectors.toList()));
            //排序
            List<LocationPriceDetailDTO> sortList = dto.getPriceDetails().stream().sorted((f, s) -> (int) (f.getId() - s.getId())).collect(Collectors.toList());
            dto.setPriceDetails(sortList);
        }
        return dto;
    }
    @GetMapping("/locationPrice")
    public ResponseEntity<Object> locationPrice() {

        OpLocationPileGroupEntity entity = new OpLocationPileGroupEntity();
        for (AllocationStrategyEnums strategyEnum  : AllocationStrategyEnums.values() ) {
            entity.setAllocationStrategy(String.valueOf(strategyEnum.getValue()));
            entity.setSmartChargingMode(null);
            entity.setMinReserve(null);
            opLocationPileGroupMapper.update(entity, Wrappers.lambdaUpdate(OpLocationPileGroupEntity.class).eq(OpLocationPileGroupEntity::getAllocationStrategy, strategyEnum.getCode()));
        }
        LambdaQueryWrapper<OpLocationPileGroupEntity> aNull = Wrappers.lambdaQuery(OpLocationPileGroupEntity.class).isNull(OpLocationPileGroupEntity::getPrices).eq(OpLocationPileGroupEntity::getEnergyUseStrategy, 1);
        List<OpLocationPileGroupEntity> opLocationPileGroupEntities = opLocationPileGroupMapper.selectList(aNull);
        for(OpLocationPileGroupEntity opLocationPileGroupEntity : opLocationPileGroupEntities) {
            if(Objects.nonNull(opLocationPileGroupEntity.getLocationId())) {


                List<OpLocationPriceVO> priceVOList = opLocationPriceService.getList(opLocationPileGroupEntity.getLocationId());
                if (!CollectionUtils.isEmpty(priceVOList)) {
                    log.error("details, getList={}", JSON.toJSONString(priceVOList));
                    LocationPriceDTO dto = toLocationPriceDto(priceVOList.get(0));
                    log.info(JSON.toJSONString(dto));
                    List<LocationPriceDetailDTO> priceDetails = dto.getPriceDetails();
                    List<ElectricityPrice> electricityPriceList = new ArrayList<>();
                    int index = 1;
                    for (LocationPriceDetailDTO  ds : priceDetails) {
                        ElectricityPrice item = new ElectricityPrice();
                        item.setId(index++);
                        item.setDays(ds.getDays());
                        item.setDetails(new ArrayList<>());
                        List<LocationPriceDetailDTO.DayPriceDTO> dayPriceList = ds.getDayPriceList();
                        for (LocationPriceDetailDTO.DayPriceDTO  p : dayPriceList) {
                            DailyPrice dailyPrice = new DailyPrice();
                            dailyPrice.setStartTime(p.getStartTime());
                            dailyPrice.setEndTime(p.getEndTime());
                            dailyPrice.setPrice(p.getPrice());
                            item.getDetails().add(dailyPrice);
                        }
                        electricityPriceList.add(item);
                    }

                    opLocationPileGroupEntity.setPrices(electricityPriceList);
                    log.info(JSON.toJSONString(electricityPriceList));
                    opLocationPileGroupMapper.updateById(opLocationPileGroupEntity);
                }
            }
        }
        return ResponseEntity.ok(opLocationPileGroupEntities.size());
    }

    @PostMapping("/cleanUp")
    @ApiOperation("删除缓存")
    public Result<Void> cleanUp() {
        pileUserServiceAdapter.cleanUp();
        return Result.ofSucceed();
    }

    @Resource
    private OpLocationPileVipConfigMapper opLocationPileVipConfigMapper;

    @PostMapping("batch")
    @ApiOperation("删除缓存")
    public Result<Object> batch(@RequestBody List<OpLocationPileVipConfigEntity> entities) {

        OpLocationPileVipConfigEntity entity = entities.get(0);
        int a = opLocationPileVipConfigMapper.insertOrUpdate(entities.get(0));

        int b = opLocationPileVipConfigMapper.batchAddOrUpdate(entities);

        LambdaQueryWrapper<OpLocationPileVipConfigEntity> queryWapper = Wrappers.lambdaQuery(OpLocationPileVipConfigEntity.class);
        queryWapper.eq(OpLocationPileVipConfigEntity::getGroupId, entity.getGroupId())
                .eq(OpLocationPileVipConfigEntity::getPrincipalId, entity.getPrincipalId())
                .eq(OpLocationPileVipConfigEntity::getType, entity.getType());
        return Result.ofSucceed(opLocationPileVipConfigMapper.selectOne(queryWapper));
    }


    @PostMapping("/refreshSeller")
    @ApiOperation("删除缓存")
    public Result<Void> cleanUp(@RequestParam("merchantId") long merchantId) {
        pileUserServiceAdapter.refreshSeller(merchantId);
        return Result.ofSucceed();
    }

    @PostMapping("/updateEvseStateByUpdatedAt")
    @ApiOperation("更新充电设备状态 压力测试使用")
    public Result<Boolean> updateEvseStateByUpdatedAt(@RequestParam("evseSn") String evseSn, @RequestParam("state") String state, @RequestParam("updatedAt") Long updatedAt) {
        return Result.ofSucceed(opLocationEvseRepository.updateEvseStateByUpdateAt(evseSn, state, updatedAt));
    }

    @GetMapping("/do/sen2d")
    public ResponseEntity<Object> send2() {
        for (int i = 0; i < 1000; i++) {
//            rabbitTemplate.convertAndSend("energy.monitor.sync.haoran.retry.exchange","haoran", i+"");
            rabbitTemplate.convertAndSend("DIRECT_EXCHANGE_CHARGE_EVENT_START_CHARGE_CALLBACK" + RabbitBean.RABBITMQ_VERSION_SUFFIX, "CHARGE.EVENT.START_CHARGE_CALLBACK", new HubPileDTO(), messagePostProcessor);
        }
        return null;
    }

    @GetMapping("/es/location")
    public Result<List<OpLocationElasticDTO>> esLocation() {
        log.info("TestController.esLocation start and userId = " + UserUtil.getUserId());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        Iterable<OpLocationElasticDTO> iterable =
//                opLocationElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<OpLocationElasticDTO> list = new ArrayList<>();
        iterable.forEach(list::add);
        return Result.ofSucceed(list);
    }

    @GetMapping("/es/locationEvse")
    public Result<List<OpLocationEvseElasticDTO>> esLocationEvse() {
        log.info("TestController.esLocationEvse start and userId = " + UserUtil.getUserId());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationEvseElasticDTO> list = new ArrayList<>();
        iterable.forEach(list::add);
        return Result.ofSucceed(list);
    }

    @GetMapping("/es/locationPileEvse")
    public Result<List<OpLocationPileEvseElasticDTO>> esLocationPileEvse() {
        log.info("TestController.esLocationPileEvse start and userId = " + UserUtil.getUserId());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        Iterable<OpLocationPileEvseElasticDTO> iterable =
//                opLocationPileEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        List<OpLocationPileEvseElasticDTO> list = new ArrayList<>();
        iterable.forEach(list::add);
        return Result.ofSucceed(list);
    }

    @GetMapping("/es/locationGroupAssociate")
    public Result<List<OpLocationPileGroupAssociateElasticDTO>> esLocationGroupAssociate() {
        log.info("TestController.esLocationGroupAssociate start and userId = " + UserUtil.getUserId());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        Iterable<OpLocationPileGroupAssociateElasticDTO> iterable =
//                opLocationPileGroupAssociateElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationPileGroupAssociateElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());

        List<OpLocationPileGroupAssociateElasticDTO> list = new ArrayList<>();
        iterable.forEach(list::add);
        return Result.ofSucceed(list);
    }

    @DeleteMapping("/es/deleteLocationById/{id}")
    public Result<Boolean> deleteLocationById(@PathVariable("id") Long id) {
        log.info("TestController.deleteLocationById start and id = " + id + "  and userId = " + UserUtil.getUserId());
        opLocationElastic.deleteById(id);
        return Result.ofSucceed(Boolean.TRUE);
    }

    @DeleteMapping("/es/deletePileByPileSn/{pileSn}")
    public Result<Boolean> deletePileByPileSn(@PathVariable("pileSn") String pileSn) {
        log.info("TestController.deletePileByPileSn start and pileSn = " + pileSn + " and userId = " + UserUtil.getUserId());
        BoolQueryBuilder queryBuilder = QueryBuilders.boolQuery();
        queryBuilder.filter(QueryBuilders.termsQuery("pileSn", pileSn));
        Iterable<OpLocationPileEvseElasticDTO> pileIterable =
//                opLocationPileEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationPileEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        pileIterable.forEach(pile -> opLocationPileEvseElastic.deleteById(pile.getId()));
        Iterable<OpLocationEvseElasticDTO> iterable =
//                opLocationEvseElastic.search(queryBuilder);
                elasticsearchRestTemplate.search(new NativeSearchQueryBuilder().withQuery(queryBuilder).build(), OpLocationEvseElasticDTO.class)
                        .stream().map(SearchHit::getContent).collect(Collectors.toList());
        iterable.forEach(evse -> opLocationEvseElastic.deleteById(evse.getId()));
        return Result.ofSucceed(Boolean.TRUE);
    }


    @GetMapping("/getLoginUserInfo")
    public Result<JSONObject> getLoginUserInfo() {
        log.info("TestController.getLoginUserInfo start and userId = " + UserUtil.getUserId());
        HttpServletRequest request = ((ServletRequestAttributes) Objects
                .requireNonNull(RequestContextHolder.getRequestAttributes())).getRequest();
        String info = request.getHeader("user-info");
        JSONObject jsonObject = JSON.parseObject(info);
        return Result.ofSucceed(jsonObject);
    }

    @GetMapping("/deleteESLocation")
    public Result<String> deleteEsLocation() {
        log.info("TestController.deleteEsLocation start and userId = " + UserUtil.getUserId());
        opLocationElastic.deleteAll();
        return Result.ofSucceed();
    }

    @GetMapping("/deleteESPile")
    public Result<String> deleteEsPile() {
        log.info("TestController.deleteEsPile start and userId = " + UserUtil.getUserId());
        opLocationPileEvseElastic.deleteAll();
        return Result.ofSucceed();
    }

    @GetMapping("/deleteESEvse")
    public Result<String> deleteEsEvse() {
        log.info("TestController.deleteEsEvse start and userId = " + UserUtil.getUserId());
        opLocationEvseElastic.deleteAll();
        return Result.ofSucceed();
    }

    @GetMapping("/deleteESAssociate")
    public Result<String> deleteEsAssociate() {
        log.info("TestController.deleteEsAssociate start and userId = " + UserUtil.getUserId());
        opLocationPileGroupAssociateElastic.deleteAll();
        return Result.ofSucceed();
    }

    @GetMapping("/deletedDataNotInMysql")
    public Result<Boolean> deletedDataNotInMysql() {
        log.info("TestController.deletedDataNotInMysql start and userId = " + UserUtil.getUserId());
        try {
            Page<OpLocationElasticDTO> locationPage;
            int page = 0;
            do {
                Pageable pageable = PageRequest.of(page++, 100);
                locationPage = opLocationElastic.findAll(pageable);
                log.info("deletedDataNotInMysql and locationPage = " + JSON.toJSONString(locationPage));
                List<OpLocationElasticDTO> list = locationPage.getContent();
                list.forEach(location -> {
                    OpLocationEntity mysql = opLocationRepository.getById(location.getId());
                    if (mysql == null || mysql.getDeleted() == 1) {
                        log.info("TestController.deletedDataNotInMysql and location = " + JSON.toJSONString(location));
                        opLocationElastic.deleteById(location.getId());
                    }
                });
            } while (locationPage.getTotalPages() > page);
            Page<OpLocationPileEvseElasticDTO> pilePage;
            page = 0;
            do {
                Pageable pageable = PageRequest.of(page++, 100);
                pilePage = opLocationPileEvseElastic.findAll(pageable);
                log.info("deletedDataNotInMysql and pilePage = " + JSON.toJSONString(pilePage));
                List<OpLocationPileEvseElasticDTO> list = pilePage.getContent();
                list.forEach(pile -> {
                    OpLocationPileEvseEntity mysql = opLocationPileEvseRepository.getById(pile.getId());
                    if (mysql == null || mysql.getDeleted() == 1) {
                        log.info("TestController.deletedDataNotInMysql and pile = " + JSON.toJSONString(pile));
                        opLocationPileEvseElastic.deleteById(pile.getId());
                    }
                });
            } while (pilePage.getTotalPages() > page);

            Page<OpLocationEvseElasticDTO> evsePage;
            page = 0;
            do {
                Pageable pageable = PageRequest.of(page++, 100);
                evsePage = opLocationEvseElastic.findAll(pageable);
                log.info("deletedDataNotInMysql and evsePage = " + JSON.toJSONString(evsePage));
                List<OpLocationEvseElasticDTO> list = evsePage.getContent();
                list.forEach(evse -> {
                    OpLocationEvseEntity mysql = opLocationEvseRepository.getById(evse.getId());
                    if (mysql == null || mysql.getDeleted() == 1) {
                        log.info("TestController.deletedDataNotInMysql and evse = " + JSON.toJSONString(evse));
                        opLocationEvseElastic.deleteById(evse.getId());
                    }
                });
            } while (evsePage.getTotalPages() > page);

            Page<OpLocationPileGroupAssociateElasticDTO> associatePage;
            page = 0;
            do {
                Pageable pageable = PageRequest.of(page++, 100);
                associatePage = opLocationPileGroupAssociateElastic.findAll(pageable);
                log.info("deletedDataNotInMysql and associatePage = " + JSON.toJSONString(associatePage));
                List<OpLocationPileGroupAssociateElasticDTO> list = associatePage.getContent();
                list.forEach(associate -> {
                    OpLocationPileGroupAssociateEntity mysql = opLocationPileGroupAssociateRepository.getById(associate.getId());
                    if (mysql == null || mysql.getDeleted()) {
                        log.info("TestController.deletedDataNotInMysql and associate = " + JSON.toJSONString(associate));
                        opLocationPileGroupAssociateElastic.deleteById(associate.getId());
                    }
                });
            } while (associatePage.getTotalPages() > page);
            return Result.ofSucceed(Boolean.TRUE);
        } catch (Exception e) {
            log.error("TestController.deletedDataNotInMysql and exception = ", e);
            return Result.ofSucceed(Boolean.FALSE);
        }
    }

    @PostMapping("/chargingCost999Template")
    public Result<CostModelRuleDTO> chargingCost999Template(@RequestBody CostModelRuleDTO resourceCostModelRuleDTO) {
        return Result.ofSucceed(TariffUtil.chargingCost999Template(resourceCostModelRuleDTO));
    }

    @GetMapping(value = "/timezone")
    public Result<TimezoneUserDTO> getTimezone(@RequestParam("userId") Long userId) {
        return Result.ofSucceed(userTimezoneServiceImpl.getTimeZoneInfoOfUser(userId));
    }

    @PutMapping(value = "/setTimezone")
    public Result<Boolean> setTimezone(@RequestBody TimezoneUserDTO timezoneUserDTO) {
        return Result.ofSucceed(userTimezoneServiceImpl.setTimezone(timezoneUserDTO));
    }

    @GetMapping("/subscribedInvoice")
    public boolean sendSubscribedInvoice(@RequestParam("email") String email) {
        final SubscribedInvoiceDTO subscribedInvoiceDTO = new SubscribedInvoiceDTO();
        subscribedInvoiceDTO.setInvoiceNo("45");
        subscribedInvoiceDTO.setInvoiceDate("2023-02-13");
        subscribedInvoiceDTO.setOrderId("123456L");
        subscribedInvoiceDTO.setTimeZone("UTF-8");
        subscribedInvoiceDTO.setCustomerId(435464L);
        subscribedInvoiceDTO.setCustomerName("HANSKJSKF");
        subscribedInvoiceDTO.setCustomerAddress("深圳市");
        subscribedInvoiceDTO.setCustomerCountryCode("CHINA");
        subscribedInvoiceDTO.setVatNumber("11111L");
        subscribedInvoiceDTO.setPaymentDate("2023-06-13");
        final SubscribeInfoDetail subscribeInfoDetail1 = new SubscribeInfoDetail();
        subscribeInfoDetail1.setCurrencySymbol("$");
        subscribeInfoDetail1.setSubTotal(BigDecimal.valueOf(123.000));
        subscribeInfoDetail1.setTotalAmount(BigDecimal.valueOf(63.23));
        subscribeInfoDetail1.setVat(BigDecimal.valueOf(666.124));
        subscribeInfoDetail1.setTotalDiscount(BigDecimal.valueOf(666.124));
        subscribeInfoDetail1.setSalesTax(BigDecimal.valueOf(587.4));
        subscribeInfoDetail1.setTotalTax(BigDecimal.valueOf(52.678));
        List<OrderCommodityDetail> orderCommodityDetails1 = new ArrayList<>();
        final OrderCommodityDetail orderCommodityDetail = new OrderCommodityDetail();
        orderCommodityDetail.setSubTotal(BigDecimal.valueOf(123.000));
        orderCommodityDetail.setUnitPrice(BigDecimal.valueOf(66.66));
        orderCommodityDetail.setQuantity(5);
        orderCommodityDetail.setPortOrGunType("AC");
        orderCommodityDetail.setSubscriptionDuration("2年");
        orderCommodityDetail.setCommodityName("NBPLUS");
        orderCommodityDetail.setCurrencySymbol("$");
        orderCommodityDetails1.add(orderCommodityDetail);
        final OrderCommodityDetail orderCommodityDetail1 = new OrderCommodityDetail();
        orderCommodityDetail1.setSubTotal(BigDecimal.valueOf(123.000));
        orderCommodityDetail1.setUnitPrice(BigDecimal.valueOf(66.66));
        orderCommodityDetail1.setQuantity(5);
        orderCommodityDetail1.setPortOrGunType("AC");
        orderCommodityDetail1.setSubscriptionDuration("2年");
        orderCommodityDetail1.setCommodityName("NBPLUS");
        orderCommodityDetail1.setCurrencySymbol("$");
        orderCommodityDetails1.add(orderCommodityDetail1);
        subscribeInfoDetail1.setOrderCommodityDetails(orderCommodityDetails1);
        subscribedInvoiceDTO.setSubscribeInfoDetail(subscribeInfoDetail1);
        //查询商家的邮箱信息
        String html = "";
        //加载模板
        org.springframework.core.io.Resource resource = new ClassPathResource("template/subscribe_invoice.html");
        try (InputStream resourceAsStream = resource.getInputStream()) {
            Document document = Jsoup.parse(resourceAsStream, CHARSET_NAME, "");
            Element body = document.body();
            StringBuilder htmlText = new StringBuilder();
            String invoiceText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(INVOICE, "Invoice")).append("#").append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getInvoiceNo()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String invoiceDateText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(INVOICE_DATE, "Invoice Date")).append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getInvoiceDate()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String billToText = htmlText.append(MessageSourceHolder.getMessage(BILL_TO, "Bill To")).append(COLON).toString();
            htmlText = new StringBuilder();
            String customerNameText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_NAME, "Customer Name")).append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerName()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String customerAddressText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_ADDRESS, "Customer Address")).append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerAddress()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String customerCountryCodeText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(CUSTOMER_COUNTRY_CODE, "Country")).append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getCustomerCountryCode()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String vatNumberText = htmlText.append(SPAN_S).append(MessageSourceHolder.getMessage(VAT_NUMBER, "Vat Number")).append(COLON).append(SPAN_E)
                    .append(SPAN_S).append(subscribedInvoiceDTO.getVatNumber()).append(SPAN_E).toString();
            htmlText = new StringBuilder();
            String orderIdText = htmlText.append(MessageSourceHolder.getMessage(ORDER_ID, "Order Id")).append(COLON).append(subscribedInvoiceDTO.getOrderId()).toString();
            htmlText = new StringBuilder();
            String paymentDateText = htmlText.append(MessageSourceHolder.getMessage(PAYMENT_DATE, "Payment Date")).append(COLON).append(subscribedInvoiceDTO.getPaymentDate()).toString();

            //表头
            htmlText = new StringBuilder();
            String commodityNameText = htmlText.append("<th class=\"p-0-16\" style=\"width: '25%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(COMMODITY_NAME, "Commodity Name")).append(SPAN_E).append(TH_E).toString();
            htmlText = new StringBuilder();
            String subscriptionDurationText = htmlText.append("<th class=\"p-0-16\" style=\"width: '15%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(SUBSCRIPTION_DURATION, "Subscription Duration")).append(SPAN_E).append(TH_E).toString();
            htmlText = new StringBuilder();
            String portTypeText = htmlText.append("<th class=\"p-0-16\" style=\"width: '15%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(PORT_TYPE, "Port Type")).append(SPAN_E).append(TH_E).toString();
            htmlText = new StringBuilder();
            String quantityText = htmlText.append("<th class=\"p-0-16\" style=\"width: '20%'\">").append(SPAN_S).append(MessageSourceHolder.getMessage(QUANTITY, "Quantity")).append(SPAN_E).append(TH_E).toString();
            htmlText = new StringBuilder();
            String unitPriceText = htmlText.append("<th class=\"p-0-16\" style=\" width: 100px \">").append(SPAN_S).append(MessageSourceHolder.getMessage(UNIT_PRICE, "Unit Price")).append(SPAN_E).append(TH_E).toString();
            htmlText = new StringBuilder();
            String subTotalText = htmlText.append("<th class=\"p-0-16\" id=\"subTotal\" >").append(SPAN_S).append(MessageSourceHolder.getMessage(SUB_TOTAL, "SubTotal")).append(SPAN_E).append(TH_E).toString();
            String tableTitle = "<tr class=\"item-wrapper\" id=\"tableTitle\">" + commodityNameText + subscriptionDurationText + portTypeText + quantityText + unitPriceText + subTotalText + TR_E;

            //订单明细信息
            htmlText = new StringBuilder();
            final SubscribeInfoDetail subscribeInfoDetail = subscribedInvoiceDTO.getSubscribeInfoDetail();
            final List<OrderCommodityDetail> orderCommodityDetails = subscribeInfoDetail.getOrderCommodityDetails();
            for (OrderCommodityDetail orderDetail : orderCommodityDetails) {
                htmlText.append("<tr class=\"t-li\" id=\"orderDetail\">");
                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getCommodityName()).append(TD_E);
                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getSubscriptionDuration()).append(TD_E);
                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getPortOrGunType()).append(TD_E);
                htmlText.append(ORDER_DETAIL_TD_S).append(orderDetail.getQuantity()).append(TD_E);
                htmlText.append("<td style=\"border-bottom: 1px solid #e5e5e5;width: 100px\">").append(subscribeInfoDetail.getCurrencySymbol()).append(orderDetail.getUnitPrice().stripTrailingZeros().toPlainString()).append(TD_E);
                htmlText.append(ORDER_DETAIL_TD_S).append(subscribeInfoDetail.getCurrencySymbol()).append(orderDetail.getSubTotal().stripTrailingZeros().toPlainString()).append(TD_E);
                htmlText.append(TR_E);
            }
            String orderDetailText = htmlText.toString();
            //小计
            htmlText = new StringBuilder();
            final String currencySymbol = subscribedInvoiceDTO.getCurrencySymbol();
            String totalText = htmlText.append(MessageSourceHolder.getMessage(TOTAL, "total")).toString();
            htmlText = new StringBuilder();
            String totalValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getSubTotal().stripTrailingZeros().toPlainString()).toString();
            //折扣
            htmlText = new StringBuilder();
            String discountText = htmlText.append(MessageSourceHolder.getMessage(DISCOUNT_AMOUNT, "discount")).toString();
            htmlText = new StringBuilder();
            String discountValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalDiscount().stripTrailingZeros().toPlainString()).toString();
            //销售税
            htmlText = new StringBuilder();
            final String salesTaxText = htmlText.append(MessageSourceHolder.getMessage(SALES_TAX, "Sales Tax")).toString();
            htmlText = new StringBuilder();
            htmlText.append(currencySymbol).append(subscribeInfoDetail.getSalesTax().stripTrailingZeros().toPlainString());
            final List<BigDecimal> salesTaxRates = subscribedInvoiceDTO.getSalesTaxRates();
//            addTaxLabel(htmlText, salesTaxRates);
            String salesTaxValueText = htmlText.toString();

            //增值税
            htmlText = new StringBuilder();
            final String vatText = htmlText.append(MessageSourceHolder.getMessage(VAT, "Vat")).toString();
            htmlText = new StringBuilder();
            htmlText.append(currencySymbol).append(subscribeInfoDetail.getVat().stripTrailingZeros().toPlainString());
            final List<BigDecimal> vatRates = subscribedInvoiceDTO.getVatRates();
//            addTaxLabel(htmlText, vatRates);
            String vatValueText = htmlText.toString();

            //总税额
            htmlText = new StringBuilder();
            String totalTaxText = htmlText.append(MessageSourceHolder.getMessage(TOTAL_TAX, "Total Tax")).toString();
            htmlText = new StringBuilder();
            final String totalTaxValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalTax().stripTrailingZeros().toPlainString()).toString();

            //总计
            htmlText = new StringBuilder();
            String totalAmountText = htmlText.append(MessageSourceHolder.getMessage(TOTAL_AMOUNT, "Total Amount")).toString();
            htmlText = new StringBuilder();
            final String totalAmountValueText = htmlText.append(currencySymbol).append(subscribeInfoDetail.getTotalAmount().stripTrailingZeros().toPlainString()).toString();

            //公司名称
            htmlText = new StringBuilder();
            String companyNameText = htmlText.append(subscribedInvoiceDTO.getCompanyName()).toString();
            //公司地址
            htmlText = new StringBuilder();
            String companyAddrText = htmlText.append(subscribedInvoiceDTO.getCompanyAddress()).toString();

            //将拼接好的内容放入标签
            fillLabels(body, "invoiceNo", invoiceText);
            fillLabels(body, "invoiceDate", invoiceDateText);
            fillLabels(body, "billTo", billToText);
            fillLabels(body, "customerName", customerNameText);
            fillLabels(body, "customerAddress", customerAddressText);
            fillLabels(body, "customerCountryCode", customerCountryCodeText);
            fillLabels(body, "vatNumber", vatNumberText);
            fillLabels(body, "orderId", orderIdText);
            fillLabels(body, "paymentDate", paymentDateText);

            //清空表体
            Objects.requireNonNull(body.getElementById("dataTable")).empty();
            fillLabels(body, "dataTable", TBODY_S + tableTitle + orderDetailText + TBODY_E);

            fillLabels(body, "total", totalText);
            fillLabels(body, "totalValue", totalValueText);

            fillLabels(body, "discount", discountText);
            fillLabels(body, "discountValue", discountValueText);

            fillLabels(body, "salesTax", salesTaxText);
            fillLabels(body, "salesTaxValue", salesTaxValueText);

            fillLabels(body, "vat", vatText);
            fillLabels(body, "vatTaxValue", vatValueText);

            fillLabels(body, "totalTax", totalTaxText);
            fillLabels(body, "totalTaxValue", totalTaxValueText);

            fillLabels(body, "totalAmount", totalAmountText);
            fillLabels(body, "totalAmountValue", totalAmountValueText);

            fillLabels(body, "companyName", companyNameText);
            fillLabels(body, "companyAddress", companyAddrText);
            html = document.html();
        } catch (IOException e) {
            log.error("IoException", e);
//            sendMessage(nacosNs + "发送发票失败，订单ID：" + orderId + e.getMessage());
            throw new MessageCodeException(PileBillExceptionEnum.INVOICE_PDF);
        }
        log.info("html\n:{}", html);
        EmailSendDTO emailSendDto = new EmailSendDTO();
        emailSendDto.setContent(html);
//        emailSendDto.setEmail(sellerDetailVO.getEmail());
        emailSendDto.setSubject(MessageSourceHolder.getMessage(SUBJECT, "Invoice"));
        emailSendDto.setSenderName(SENDER);
//        threadPoolTaskExecutor.submit(new RunnableWrapper(() -> {
//            try {
//                commonService.sendEmail(emailSendDto);
//            } catch (Exception e) {
//                log.info("邮件发送失败", e);
//            }
//        }));
        return true;
    }

    private void fillLabels(Element body, String invoiceNo, String invoiceText) {
        body.getElementById(invoiceNo).html(invoiceText);
    }

    @GetMapping("/test1111")
    public SubscribeInfoDetail test(@RequestParam(defaultValue = "0.00000000000001") String num) {
        SubscribeInfoDetail subscribeInfoDetail = new SubscribeInfoDetail();
        List<BigDecimal> rates = new ArrayList<>();
        rates.add(new BigDecimal(num));
        rates.add(new BigDecimal(num));
        rates.add(new BigDecimal(num));
        subscribeInfoDetail.setVatRates(rates);
        subscribeInfoDetail.setVat(new BigDecimal(num));
        return subscribeInfoDetail;
    }

    /**
     * 桩是否可用
     *
     * @param pileSn
     * @return
     */
    //@ApiOperation(value = "test thread pool mdc")
    @GetMapping("/pile/test")
    Result<String> testMDC(@RequestParam("pileSn") String pileSn) {
        log.info("--->>> test log mdc start.");
        ThreadPoolTaskExecutor threadPoolTaskExecutor = threadPoolTaskExeWithMdc.taskExecutor();
        threadPoolTaskExecutor.submit(() -> log.info("------>>>>>>>>>5555555"));
        log.info("--->>> test log mdc end.");
        return Result.ofSucceed("success");
    }

    @GetMapping("/repairRedisOfOperatorIdByPileSN")
    Result<Integer> repairRedisOfOperatorIdByPileSN() {
        return Result.ofSucceed(opLocationPileEvseRepository.repairRedisOfOperatorIdByPileSN());
    }


    @GetMapping("/subs/test")
    @ApiOperation("订阅测试")
    Result<Boolean> subsTest() {

        AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> {
           log.info("Async method. 5/0 start.");
           int a = 5/0;
           log.info("error: a= {}", a);
        }));

        log.info("Async method. 5/0 end.");

        //1788479474699145217
      /*  TbHonourAgreementEntity honourAgreementEntity = new TbHonourAgreementEntity();
        honourAgreementEntity.setOrderId("1791740614658228225");
        honourAgreementEntity.setTenantId("1530749400295227393");
        subscribePlaceOrderService.getAndBindLicence(honourAgreementEntity);*/
        return Result.ofSucceed(Boolean.TRUE);
    }

    @GetMapping("/subs/testWithCatch")
    @ApiOperation("订阅测试 catch error")
    Result<Boolean> testWithCatch() {

        AutelThreadUtils.THREAD_POOL.execute(RunnableWrapper.of(() -> {
            try {
                log.info("Async method. 15/0 start.");
                int a = 15/0;
                log.info("error: a= {}", a);
            } catch (Exception e) {
                log.error("testWithCatch error.", e);
            }
        }));

        log.info("Async method. 15/0 end.");

        //1788479474699145217
      /*  TbHonourAgreementEntity honourAgreementEntity = new TbHonourAgreementEntity();
        honourAgreementEntity.setOrderId("1791740614658228225");
        honourAgreementEntity.setTenantId("1530749400295227393");
        subscribePlaceOrderService.getAndBindLicence(honourAgreementEntity);*/
        return Result.ofSucceed(Boolean.TRUE);
    }


}
