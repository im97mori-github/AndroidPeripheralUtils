package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.Device;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Flowable;
import io.reactivex.rxjava3.processors.PublishProcessor;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class FakeMainViewModel extends MainViewModel {

    PublishProcessor<List<Device>> pp = PublishProcessor.create();

    @Inject
    FakeMainViewModel(@NonNull DeviceRepository deviceRepository) {
        super(deviceRepository);
    }

    @NonNull
    public Flowable<List<Device>> getDeviceList() {
        return pp;
    }

}