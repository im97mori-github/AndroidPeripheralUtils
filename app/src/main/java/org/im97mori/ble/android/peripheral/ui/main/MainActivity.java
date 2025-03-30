package org.im97mori.ble.android.peripheral.ui.main;

import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_ID;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.VALUE_DEVICE_ID_UNSAVED;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.result.ActivityResultLauncher;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.util.Pair;
import androidx.core.view.MenuProvider;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import com.google.android.gms.oss.licenses.OssLicensesMenuActivity;

import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.databinding.MainActivityBinding;
import org.im97mori.ble.android.peripheral.ui.device.PeripheralActivity;
import org.im97mori.ble.android.peripheral.ui.device.setting.DeviceSettingLauncherContract;
import org.im97mori.ble.android.peripheral.ui.device.type.DeviceListLauncherContract;
import org.im97mori.ble.android.peripheral.utils.AutoDisposeViewModelProvider;
import org.im97mori.stacklog.LogUtils;

import java.util.Collections;
import java.util.LinkedList;
import java.util.function.Function;

import dagger.hilt.android.AndroidEntryPoint;

import javax.inject.Inject;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {

    @Inject
    Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> viewModelProviderFactoryFunction;

    private MainViewModel mViewModel;

    private MainActivityBinding mBinding;

    private DeviceListAdapter adapter;

    private final ActivityResultLauncher<Pair<Long, Integer>> mStartDeviceSettingActivity = registerForActivityResult(new DeviceSettingLauncherContract(), result -> {
    });

    private final ActivityResultLauncher<Void> mStartDeviceTypeListActivity = registerForActivityResult(new DeviceListLauncherContract(), result -> {
        if (result != null) {
            mStartDeviceSettingActivity.launch(Pair.create(VALUE_DEVICE_ID_UNSAVED, result));
        }
    });

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        mViewModel = new AutoDisposeViewModelProvider(this, viewModelProviderFactoryFunction.apply(this)).get(MainViewModel.class);
        mBinding = MainActivityBinding.inflate(getLayoutInflater());
        setContentView(mBinding.getRoot());

        mBinding.grid.setEmptyView(mBinding.empty);
        adapter = new DeviceListAdapter(this, mViewModel.provideDeviceTypeImageResMap(), Collections.synchronizedList(new LinkedList<>()));
        mBinding.grid.setAdapter(adapter);
        mBinding.grid.setOnItemClickListener((parent, view, position, id) -> {
            Intent intent = new Intent(getApplicationContext(), PeripheralActivity.class);
            intent.putExtra(KEY_DEVICE_ID, adapter.getItemId(position));
            startActivity(intent);
        });

        mBinding.topAppBar.setOnMenuItemClickListener(this::onOptionsItemSelected);
        mBinding.topAppBar.addMenuProvider(new MenuProvider() {
            @Override
            public void onCreateMenu(@NonNull Menu menu, @NonNull MenuInflater menuInflater) {
                menu.setGroupVisible(R.id.all, mBinding.rootContainer.getVisibility() == View.VISIBLE);
            }

            @Override
            public boolean onMenuItemSelected(@NonNull MenuItem menuItem) {
                boolean result = false;
                if (R.id.create_device == menuItem.getItemId()) {
                    mStartDeviceTypeListActivity.launch(null);
                    result = true;
                } else if (R.id.clear_devices == menuItem.getItemId()) {
                    mViewModel.observeDeleteAllDeviceSetting(() -> {
                    }, throwable -> LogUtils.stackLog(throwable.getMessage()));
                    result = true;
                } else if (R.id.license == menuItem.getItemId()) {
                    startActivity(new Intent(getApplicationContext(), OssLicensesMenuActivity.class));
                    result = true;
                }
                return result;
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        mViewModel.observeLoadAllDeviceSetting(devices -> {
            adapter.setDeviceList(devices);
            mBinding.rootContainer.setVisibility(View.VISIBLE);
            mBinding.topAppBar.invalidateMenu();
        }, throwable -> LogUtils.stackLog(throwable.getMessage()));
    }

}