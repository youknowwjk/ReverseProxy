package com.autel.cloud.pile.base.domain.repository.impl;

import com.autel.cloud.pile.base.domain.repository.OpEvseBrandRepository;
import com.autel.cloud.pile.base.infrastructure.mapper.OpEvseBrandMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.OpEvseBrandEntity;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

@Service
@Log4j2
public class OpEvseBrandRepositoryImpl extends ServiceImpl<OpEvseBrandMapper, OpEvseBrandEntity> implements OpEvseBrandRepository {

}
