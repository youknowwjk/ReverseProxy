package com.autel.cloud.pile.base.domain.listener;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.pile.base.ChargePointNoticeEvent;
import com.autel.cloud.pile.base.constant.AmqpConstant;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.service.OpLocationEvseService;
import com.autel.cloud.pile.base.domain.service.RuleLocationPileService;
import com.autel.cloud.pile.base.dto.EvseExpandDTO;
import com.autel.cloud.pile.base.infrastructure.amqp.RabbitBean;
import com.autel.cloud.pile.base.infrastructure.elastic.OpLocationEvseExpandElasticService;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.RuleLocationPileEntity;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.rabbitmq.client.Channel;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.logging.log4j.ThreadContext;
import org.springframework.amqp.core.ExchangeTypes;
import org.springframework.amqp.core.Message;
import org.springframework.amqp.rabbit.annotation.Exchange;
import org.springframework.amqp.rabbit.annotation.Queue;
import org.springframework.amqp.rabbit.annotation.QueueBinding;
import org.springframework.amqp.rabbit.annotation.RabbitListener;
import org.springframework.amqp.rabbit.core.RabbitTemplate;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import javax.annotation.Resource;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import static com.autel.cloud.pile.base.constant.AmqpConstant.*;

/**
 * @Author temp
 * @Date 2022/11/10 10:18
 */
@Component
@Slf4j
public class OpLocationEvseExpandListener {

    @Resource
    private OpLocationEvseService opLocationEvseService;
    @Resource
    private RabbitTemplate rabbitTemplate;
    @Resource
    private OpLocationEvseExpandElasticService opLocationEvseExpandElasticService;

    @Resource
    private RuleLocationPileService ruleLocationPileService;

    /**
     * 场站更新同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_LOCATION_UPDATE_QUEUE +  "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_LOCATION_UPDATE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_LOCATION_UPDATE_ROUTE}))
    public void locationUpdateListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("locationUpdateListener,data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("locationUpdateListener,tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long locationId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(locationId)) {
                    opLocationEvseService.syncEvseExpand(EvseExpandDTO.builder().locationIds(Arrays.asList(locationId)).build());
                }
            }
        } catch (Exception e) {
            log.error("locationUpdateListener,exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX, PILE_BASE_LOCATION_UPDATE_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 场站新增同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_LOCATION_EXPAND_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_LOCATION_ADD_EXPAND_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_LOCATION_EXPAND_ROUTE}))
    public void locationAddListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("locationAddListener,data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("locationAddListener,tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long locationId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(locationId)) {
                    opLocationEvseService.syncEvseExpand(EvseExpandDTO.builder().locationIds(Arrays.asList(locationId)).build());
                }
            }
        } catch (Exception e) {
            log.error("locationAddListener,exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_ADD_EXPAND_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_EXPAND_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 场站删除同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = "energy.pile.base.location.delete.queue.expand"+"#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_LOCATION_DELETE_EXCHANGE+"#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_LOCATION_DELETE_ROUTE}))
    public void locationDeleteListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("locationDeleteListener,locationUpdateListener data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("locationDeleteListener,locationUpdateListener tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long locationId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(locationId)) {
                    opLocationEvseExpandElasticService.deleteByLocationIds(Arrays.asList(locationId));
                }
            }
        } catch (Exception e) {
            log.error("locationDeleteListener,locationUpdateListener exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_LOCATION_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_LOCATION_DELETE_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 枪新增同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_GUN_ADD_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_GUN_ADD_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_GUN_ADD_ROUTE}))
    public void gunAddListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("gunAddListener, data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("gunAddListener, tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long pileId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(pileId)) {
                    opLocationEvseService.syncEvseExpand(EvseExpandDTO.builder().pileIds(Arrays.asList(pileId)).build());
                }
            }
        } catch (Exception e) {
            log.error("gunAddListener, exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_GUN_ADD_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_GUN_ADD_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 枪更新同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_GUN_UPDATE_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_GUN_UPDATE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_GUN_UPDATE_ROUTE}))
    public void gunUpdateListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("gunUpdateListener, data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("gunUpdateListener, tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long pileId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(pileId)) {
                    opLocationEvseService.syncEvseExpand(EvseExpandDTO.builder().pileIds(Arrays.asList(pileId)).build());
                }
            }
        } catch (Exception e) {
            log.error("gunUpdateListener, exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_GUN_UPDATE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_GUN_UPDATE_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 枪删除同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = PILE_BASE_GUN_DELETE_QUEUE + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = PILE_BASE_GUN_DELETE_EXCHANGE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.DIRECT, ignoreDeclarationExceptions = "true"),
            key = {PILE_BASE_GUN_DELETE_ROUTE}))
    public void gunDeleteListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("gunDeleteListener, data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("gunDeleteListener, tag={}", tag);
        try {
            if (StringUtils.hasText(data)) {
                Long pileId = JSON.parseObject(data, Long.class);
                if (!ObjectUtils.isEmpty(pileId)) {
                    opLocationEvseExpandElasticService.deleteByIds(Arrays.asList(pileId));
                }
            }
        } catch (Exception e) {
            log.error("gunDeleteListener, exception:{}", e);
            rabbitTemplate.convertAndSend(PILE_BASE_GUN_DELETE_EXCHANGE + RabbitBean.RABBITMQ_VERSION_SUFFIX,PILE_BASE_GUN_DELETE_ROUTE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }

    /**
     * 桩更新同步
     * @param data
     * @param channel
     * @param message
     */
    @SneakyThrows
    @RabbitListener(bindings = @QueueBinding(value = @Queue(value = "TOPIC_PILE_BASE_PILE_NAME_QUEUE" + "#{rabbitBean.rabbitmqVersionSuffix}", durable = "true", autoDelete = "false"),
            exchange = @Exchange(value = TOPIC_EXCHANGE_PILE_BASE + "#{rabbitBean.rabbitmqVersionSuffix}", type = ExchangeTypes.TOPIC, ignoreDeclarationExceptions = "true"),
            key = {"ChargePointNoticeEvent"}))
    public void pileNameListener(String data, Channel channel, Message message) {
        ThreadContext.put(BaseConstant.THREAD_CONTEXT_ID, UUID.randomUUID().toString());
        log.info("pileNameListener, data={}", data);
        long tag = message.getMessageProperties().getDeliveryTag();
        log.info("pileNameListener, tag={}", tag);
        try {
            if (!StringUtils.hasText(data)) {
                log.info("pileNameListener, data is empty.");
                return;
            }
            ChargePointNoticeEvent pileVO = JSON.parseObject(data, ChargePointNoticeEvent.class);
            String pileSn = pileVO.getSn();
            String pileName = pileVO.getName();
            LambdaQueryWrapper<RuleLocationPileEntity> query = Wrappers.lambdaQuery();
            query.eq(RuleLocationPileEntity::getPileSn, pileSn);
            List<RuleLocationPileEntity> entityList = this.ruleLocationPileService.list(query);
            if (CollectionUtils.isEmpty(entityList)) {
                log.info("pileNameListener, entityList is empty.");
                return;
            }
            entityList.stream().forEach(e -> e.setPileName(pileName));
            this.ruleLocationPileService.updateBatchById(entityList);
        } catch (Exception e) {
            log.error("pileNameListener, exception:{}", e);
            rabbitTemplate.convertAndSend(TOPIC_EXCHANGE_PILE_BASE + RabbitBean.RABBITMQ_VERSION_SUFFIX,AmqpConstant.PILE_NAME_UPDATE,data);
        }finally {
            channel.basicAck(tag, false);
        }
    }
}
