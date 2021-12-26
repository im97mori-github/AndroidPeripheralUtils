package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;
import androidx.lifecycle.ViewModel;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;
import java.util.Map;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.disposables.CompositeDisposable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class MainViewModel extends ViewModel {

    private final DeviceRepository mDeviceRepository;

    protected final CompositeDisposable mDisposable = new CompositeDisposable();

    @Inject
    MainViewModel(@NonNull DeviceRepository deviceRepository) {
        mDeviceRepository = deviceRepository;
    }

    public void observeAllDeviceSettings(Consumer<List<DeviceSetting>> onNext, Consumer<Throwable> onError) {
        mDisposable.add(mDeviceRepository.loadAllDeviceSettings()
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onNext, onError));
    }

    public void observeDeleteAllDeviceSettings(@NonNull Action onComplete, Consumer<Throwable> onError) {
        mDisposable.add(mDeviceRepository.deleteAllDeviceSettings()
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @NonNull
    public Map<Integer, Integer> provideDeviceTypeImageResMap() {
        return mDeviceRepository.provideDeviceTypeImageResMap();
    }

    public void dispose() {
        mDisposable.dispose();
    }

}