package org.im97mori.ble.android.peripheral.ui.device.type;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.view.View;
import android.widget.TextView;
import androidx.core.util.Pair;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.BoundedMatcher;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Singleton;
import java.util.List;
import java.util.function.Function;

import static androidx.test.espresso.Espresso.*;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertNotNull;
import static org.hamcrest.Matchers.is;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_UNDEFINED;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class DeviceTypeListActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(DeviceTypeListViewModel.class, FakeDeviceTypeListViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    private ActivityScenario<DeviceTypeListActivity> mScenario;

    private FakeDeviceTypeListViewModel mViewModel;

    @Before
    public void setUp() {
        mHiltRule.inject();
        mScenario = ActivityScenario.launchActivityForResult(DeviceTypeListActivity.class);
        mScenario.onActivity(activity -> mViewModel = new ViewModelProvider(activity).get(FakeDeviceTypeListViewModel.class));
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        onView(withText(R.string.device_type)).check(matches(withParent(withId(R.id.topAppBar))));
    }

    @Test
    public void test_list_click_00001() {
        List<Pair<Integer, String>> list = mViewModel.provideDeviceTypeList();
        Pair<Integer, String> pair = list.get(0);

        onData(is(new BoundedMatcher<Object, Object>(Object.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + pair.hashCode());
            }

            @Override
            protected boolean matchesSafely(Object target) {
                return pair.equals(target);
            }
        }))
                .perform(click());
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_OK, activityResult.getResultCode());
        Intent resultIntent = activityResult.getResultData();
        assertNotNull(resultIntent);
        assertEquals(pair.first.intValue(), resultIntent.getIntExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_UNDEFINED));
    }

    @Test
    public void test_list_text_00001() {
        List<Pair<Integer, String>> list = mViewModel.provideDeviceTypeList();
        Pair<Integer, String> pair = list.get(0);

        onData(is(new BoundedMatcher<Object, Object>(Object.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + pair.hashCode());
            }

            @Override
            protected boolean matchesSafely(Object target) {
                return pair.equals(target);
            }
        }))
                .check(matches(withText(pair.second)));
    }

    @Test
    public void test_list_image_00001() {
        List<Pair<Integer, String>> list = mViewModel.provideDeviceTypeList();
        Pair<Integer, String> pair = list.get(0);

        onData(is(new BoundedMatcher<Object, Object>(Object.class) {
            @Override
            public void describeTo(Description description) {
                description.appendText("Device: " + pair.hashCode());
            }

            @Override
            protected boolean matchesSafely(Object target) {
                return pair.equals(target);
            }
        }))
                .check(matches(new TypeSafeMatcher<View>() {
                    @Override
                    protected boolean matchesSafely(View item) {
                        TextView textView = (TextView) item;
                        Bitmap targetBitmap = TestUtils.getBitmap(textView.getCompoundDrawablesRelative()[0]);
                        Integer resId = mViewModel.provideDeviceTypeImageResMap().get(pair.first);
                        assertNotNull(resId);
                        Drawable drawable = item.getContext().getDrawable(resId);
                        assertNotNull(drawable);
                        Bitmap bitmap = TestUtils.getBitmap(drawable);
                        return targetBitmap.sameAs(bitmap);
                    }

                    @Override
                    public void describeTo(Description description) {
                        description.appendText("device_type:" + pair.first);
                    }
                }));
    }

    @Test
    public void test_backPressed_00001() {
        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

}